// This file is part of Zapp, a packrat parser generator.
// Copyright (C) 2024  Daniel Grévent
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const ir = @import("low_ir.zig");
const AcceptanceSet = @import("rule_analyzer.zig").AcceptanceSet;
const gvgen = @import("graphviz_gen.zig");

const PassManager = @This();

ir: *ir.LowIr,

const PassError = Allocator.Error;

pub fn optimize(lir: *ir.LowIr) !void {
    const self: PassManager = .{
        .ir = lir,
    };

    var dfa_gen = DfaGen.init(self.ir.allocator);
    defer dfa_gen.deinit();

    const dfa = try dfa_gen.gen(self.ir.blocks.items[0]);
    const stdout = std.io.getStdOut().writer();

    try gvgen.genDfa(stdout, dfa, "dfa");
    defer dfa.deinit(self.ir.allocator);
}

fn generateDfa(self: *PassManager, block: *ir.Block) !void {
    assert(block.meta.is_terminal);
    assert(block.meta.is_target);

    _ = self;
}

/// equivalent to cloning a rule in pir
fn shallowRuleCopy(self: *PassManager, block: *ir.Block, base: usize) !*ir.Block {
    assert(block.meta.is_target);
    var map = std.AutoHashMap(usize, *ir.Block).init(self.ir.allocator);
    defer map.deinit();
    return try self.shallowRuleCopyRec(block, base, &map);
}

fn shallowRuleCopyRec(
    self: *PassManager,
    block: *ir.Block,
    base: usize,
    map: *std.AutoHashMap(usize, *ir.Block),
) !*ir.Block {
    const allocator = self.ir.allocator;
    // 3 steps
    // 1) copy the own block over or get it from the map
    if (map.get(block.id)) |clone| return clone;
    var new_block = try block.clone(allocator);
    try self.ir.appendDefBlock(new_block, base);
    try map.put(block.id, new_block);

    // 2) copy the fail block
    if (block.fail) |fail_block| {
        new_block.fail = try self.shallowRuleCopyRec(fail_block, base, map);
    }

    // 3) copy the jump to blocks
    const insts = new_block.insts.items;
    const last = &insts[insts.len - 1];
    switch (last.tag) {
        .JMP => {
            const jmp_block = last.data.jmp;
            last.data.jmp = try self.shallowRuleCopyRec(jmp_block, base, map);
        },
        .MATCH => {
            const prongs = last.data.match.items;
            for (prongs) |*prong| {
                prong.dest = try self.shallowRuleCopyRec(prong.dest, base, map);
            }
        },
        .NONTERM => {
            const jmp_block = last.data.ctx_jmp.next;
            last.data.ctx_jmp.next = try self.shallowRuleCopyRec(jmp_block, base, map);
        },
        .FAIL,
        .RET,
        .EXIT_PASS,
        .EXIT_FAIL,
        => {},
        else => unreachable,
    }

    return new_block;
}

fn blockPass(
    self: *PassManager,
    comptime pass: fn (*PassManager, *ir.Block) PassError!void,
) !void {
    const blocks = &self.ir.blocks.items;
    var i: usize = 0;
    while (i < blocks.len) : (i += 1) {
        try pass(self, blocks[i]);
    }
}

pub const Dfa = struct {
    blocks: std.ArrayList(*ir.Block),

    pub fn init(allocator: Allocator) Dfa {
        return .{
            .blocks = std.ArrayList(*ir.Block).init(allocator),
        };
    }

    pub fn deinit(self: Dfa, allocator: Allocator) void {
        for (self.blocks.items) |block| block.deinit(allocator);
        self.blocks.deinit();
    }

    pub fn getNew(self: *Dfa) !*ir.Block {
        const block = try ir.Block.init(self.blocks.allocator);
        block.id = self.blocks.items.len;
        try self.blocks.append(block);
        return block;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.blocks.items) |blk| {
            try writer.print("{s}\n", .{blk});
        }
    }
};

const DfaGen = struct {
    const Context = DfaCtx(std.hash.Wyhash);
    pub const DfaMap = std.HashMap(
        DfaState.Key,
        *ir.Block,
        Context,
        std.hash_map.default_max_load_percentage,
    );

    dfa_states: std.ArrayList(DfaState),
    map: DfaMap,
    map_keys: std.ArrayList(DfaState.Key),
    allocator: Allocator,

    pub fn init(allocator: Allocator) DfaGen {
        return .{
            .dfa_states = std.ArrayList(DfaState).init(allocator),
            .map = DfaMap.init(allocator),
            .map_keys = std.ArrayList(DfaState.Key).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn gen(self: *DfaGen, start_block: *ir.Block) !Dfa {
        var dfa = Dfa.init(self.allocator);
        const start_state = try DfaState.init(self.allocator, start_block);
        try self.dfa_states.append(start_state);
        try self.put(try start_state.toKey(), try dfa.getNew());

        const fail_block = try dfa.getNew();
        try fail_block.insts.append(ir.Instr.initTag(.FAIL));
        var new_states = std.ArrayList(DfaState).init(self.allocator);
        defer new_states.deinit();

        while (self.dfa_states.items.len != 0) {
            for (self.dfa_states.items) |*state| {
                defer state.deinit();
                const branches = try state.getBranches();
                defer branches.deinit();

                // get the block
                const curr_key = try state.toKey();
                defer curr_key.deinit(self.allocator);

                const block = self.map.get(curr_key).?;
                if (block.insts.items.len != 0) continue;

                try block.insts.append(ir.Instr.initTag(.MATCH));
                const instr = &block.insts.items[0];
                instr.data = .{ .match = std.ArrayList(ir.MatchProng)
                    .init(self.allocator) };

                if (!branches.fail_set.isEmpty()) {
                    block.fail = fail_block;
                }

                // add the prongs and get the destination block
                for (branches.prongs.items) |prong| {
                    var split_state = try state.splitOn(prong);
                    try split_state.goEps();

                    const key = try split_state.toKey();
                    defer key.deinit(self.allocator);
                    var labels = std.ArrayList(ir.Range).init(self.allocator);
                    var iter = prong.set.rangeIter();
                    while (iter.next()) |range| try labels.append(range);

                    const dst_blk = self.map.get(key) orelse blk: {
                        const dst_blk = try dfa.getNew();

                        // if the prong is not consuming the state of
                        // the block will be accepting
                        if (split_state.isEmpty()) {
                            const action = split_state.action.?;
                            var pass_instr = ir.Instr.initTag(.RET);
                            pass_instr.data = .{ .action = action };
                            try dst_blk.insts.append(pass_instr);
                        }

                        try self.put(try key.clone(self.allocator), dst_blk);

                        break :blk dst_blk;
                    };

                    try instr.data.match.append(.{
                        .labels = labels,
                        .dest = dst_blk,
                        .consuming = prong.final_action == null,
                    });

                    if (dst_blk.insts.items.len == 0) {
                        try new_states.append(split_state);
                    } else {
                        split_state.deinit();
                    }
                }
            }

            self.dfa_states.clearRetainingCapacity();
            try self.dfa_states.appendSlice(new_states.items);
            new_states.clearRetainingCapacity();
        }

        return dfa;
    }

    fn put(self: *DfaGen, key: DfaState.Key, value: *ir.Block) !void {
        try self.map_keys.append(key);
        try self.map.put(key, value);
    }

    pub fn deinit(self: *DfaGen) void {
        for (self.dfa_states.items) |dfa_state| dfa_state.deinit();
        for (self.map_keys.items) |key| key.deinit(self.allocator);

        self.dfa_states.deinit();
        self.map_keys.deinit();
        self.map.deinit();
    }
};

const DfaState = struct {
    sub_states: std.ArrayList(ExecState),
    action: ?usize,

    pub fn init(allocator: Allocator, block: *ir.Block) !DfaState {
        var sub_states = std.ArrayList(ExecState).init(allocator);
        try sub_states.append(try ExecState.init(allocator, block));

        var self: DfaState = .{
            .sub_states = sub_states,
            .action = null,
        };

        try self.goEps();
        return self;
    }

    pub fn isEmpty(self: *const DfaState) bool {
        for (self.sub_states.items) |sub| {
            if (sub.blocks.items.len != 0) return false;
        }

        return true;
    }

    pub fn goEps(self: *DfaState) !void {
        if (self.isEmpty()) return;

        while (try self.fillBranches() or try self.execJumps()) {}
        self.resetHadFill();
        for (self.sub_states.items) |*sub| {
            try sub.lookaheadGoEps();
        }

        self.deduplicate();
        if (self.isEmpty()) {
            assert(self.sub_states.items.len > 0);
            self.action = self.sub_states.items[0].last_action;
        }
    }

    fn deduplicate(self: *DfaState) void {
        var i: usize = 1;
        var found = false;
        while (i < self.sub_states.items.len) : (i += 1) {
            const sub = self.sub_states.items[i - 1];

            for (self.sub_states.items[i..]) |next| {
                if (!next.eql(sub)) continue;
                found = true;
                break;
            }

            if (found) {
                i -= 1;
                _ = self.sub_states.swapRemove(i);
                sub.deinit();
            }

            found = false;
        }
    }

    pub fn clone(self: *const DfaState) !DfaState {
        var sub_states = std.ArrayList(ExecState)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |sub_state| try sub_states.append(sub_state);

        return .{ .sub_states = sub_states, .action = self.action };
    }

    pub fn splitOn(self: *DfaState, branch: SplitBranch) !DfaState {
        var new_dfa_state: DfaState = .{
            .sub_states = std.ArrayList(ExecState)
                .init(self.sub_states.allocator),
            .action = null,
        };

        for (self.sub_states.items) |sub| {
            if (try sub.splitOn(branch.set)) |new_state| {
                try new_dfa_state.sub_states.append(new_state);
            }
        }

        if (new_dfa_state.isEmpty()) {
            assert(branch.final_action != null);
            new_dfa_state.action = branch.final_action;
        }

        return new_dfa_state;
    }

    pub const BranchResult = struct {
        fail_set: AcceptanceSet,
        prongs: std.ArrayList(SplitBranch),

        pub fn deinit(self: BranchResult) void {
            self.prongs.deinit();
        }
    };

    pub fn getBranches(self: *DfaState) !BranchResult {
        var prongs = std.ArrayList(SplitBranch)
            .init(self.sub_states.allocator);
        for (self.sub_states.items) |*state| {
            try state.addBranches(&prongs);
        }

        // augment the prongs
        var normal_match: AcceptanceSet = .{};
        try self.canonicalizeBranches(&prongs, &normal_match);

        // this strategy does not follow peg orderedness
        // and is essentially random.
        // maybe it would be nice to add a warning to the user or an error
        var i: usize = 1;
        while (i <= prongs.items.len) : (i += 1) {
            const prong = &prongs.items[i - 1];
            if (prong.final_action == null) continue;
            prong.set.cut(normal_match);
            if (prong.set.isEmpty()) {
                _ = prongs.swapRemove(i - 1);
                i -= 1;
            } else {
                normal_match.merge(prong.set);
            }
        }

        // next emit the block
        var fail_match = normal_match;
        fail_match.invert();

        return .{
            .fail_set = fail_match,
            .prongs = prongs,
        };
    }

    fn canonicalizeBranches(
        _: *DfaState,
        prongs: *std.ArrayList(SplitBranch),
        normal_match: *AcceptanceSet,
    ) !void {
        var i: usize = 1;
        while (i <= prongs.items.len) : (i += 1) {
            const prong = &prongs.items[i - 1];
            if (prong.final_action != null) continue;
            normal_match.merge(prong.set);

            var j: usize = i;
            while (j < prongs.items.len) : (j += 1) {
                const other_prong = &prongs.items[j];

                if (other_prong.final_action != null) continue;
                if (prong.set.disjoint(other_prong.set)) continue;
                if (prong.set.eql(other_prong.set)) {
                    _ = prongs.swapRemove(j);
                    j -= 1; // no problem as j > i - 1 >= 0
                    continue;
                }

                var prong_cp: SplitBranch = undefined;
                if (prong.set.subSet(other_prong.set)) {
                    prong_cp = other_prong.*;
                    prong_cp.set.cut(prong.set);
                    _ = prongs.swapRemove(j);
                    j -= 1;
                } else if (other_prong.set.subSet(prong.set)) {
                    std.mem.swap(SplitBranch, prong, other_prong);
                    prong_cp = other_prong.*;
                    prong_cp.set.cut(prong.set);
                    _ = prongs.swapRemove(j);
                    j -= 1;
                } else {
                    prong_cp = prong.*;
                    prong_cp.set.intersect(other_prong.set);
                    prong.set.cut(prong_cp.set);
                    other_prong.set.cut(prong_cp.set);

                    assert(prong.set.disjoint(prong_cp.set) and
                        prong.set.disjoint(other_prong.set) and
                        prong_cp.set.disjoint(other_prong.set));
                }

                assert(!prong_cp.set.isEmpty());
                try prongs.append(prong_cp);
            }
        }
    }

    fn resetHadFill(self: *DfaState) void {
        for (self.sub_states.items) |*sub| sub.had_fill = false;
    }

    fn execJumps(self: *DfaState) !bool {
        var had_change = false;
        for (self.sub_states.items) |*sub| {
            had_change = try sub.execJumps() or had_change;
        }

        return had_change;
    }

    fn fillBranches(self: *DfaState) !bool {
        var i: usize = 0;
        var had_change = false;
        while (i < self.sub_states.items.len) : (i += 1) {
            var state = self.sub_states.items[i];
            if (!state.canFillBranches()) continue;

            had_change = true;
            while (try state.fillBranches()) |new_state| {
                try self.sub_states.append(new_state);
            }
        }

        return had_change;
    }

    pub const Key = struct {
        sub_states: []const ExecState.Key,
        action: ?usize,

        pub fn clone(self: Key, allocator: Allocator) !Key {
            const sub_states = try allocator.dupe(ExecState.Key, self.sub_states);
            return .{ .sub_states = sub_states, .action = self.action };
        }

        pub fn deinit(self: Key, allocator: Allocator) void {
            allocator.free(self.sub_states);
        }

        pub fn eql(self: Key, other: Key) bool {
            if (self.sub_states.len != other.sub_states.len) return false;
            if (other.sub_states.len == 0) {
                return other.action == self.action;
            }

            for (self.sub_states, other.sub_states) |self_sub, other_sub| {
                if (!self_sub.eql(other_sub)) return false;
            }

            return true;
        }

        pub fn hash(self: Key, hasher: anytype) void {
            if (self.action) |val| {
                hasher.update(std.mem.asBytes(&val));
            } else {
                hasher.update(std.mem.asBytes(&@as(usize, 0)));
            }

            for (self.sub_states) |sub_state| {
                sub_state.hash(hasher);
            }
        }

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (self.sub_states.len == 0) {
                try writer.print("(∅ , {d})", .{self.action.?});
                return;
            }

            try writer.print("{{ ", .{});
            for (self.sub_states) |sub_key| {
                try writer.print("{s} ", .{sub_key});
            }
            try writer.print("}}", .{});
        }
    };

    pub fn toKey(self: DfaState) !Key {
        var sub_states = std.ArrayList(ExecState.Key)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |state| {
            if (try state.toKey()) |key| {
                try sub_states.append(key);
            }
        }

        return .{
            .sub_states = try sub_states.toOwnedSlice(),
            .action = self.action,
        };
    }

    pub fn deinit(self: DfaState) void {
        for (self.sub_states.items) |sub_state| sub_state.deinit();
        self.sub_states.deinit();
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.isEmpty()) {
            try writer.print("(∅ , {d})", .{self.action.?});
            return;
        }

        try writer.print("{{ ", .{});
        for (self.sub_states.items) |sub_state| {
            try writer.print("{s} ", .{sub_state});
        }
        try writer.print("}}", .{});
    }
};

const LookaheadState = struct {
    base: DfaState,
    is_positive: bool,

    pub const Key = struct {
        base: DfaState.Key,
        is_positive: bool,

        pub fn deinit(self: Key) void {
            self.base.deinit();
        }

        pub fn eql(self: *const Key, other: *const Key) bool {
            return self.base.eql(other.base) and
                self.is_positive == other.is_positive;
        }

        pub fn hash(self: Key, hasher: anytype) void {
            self.base.hash(hasher);
            assert(std.meta.hasUniqueRepresentation(bool));
            hasher.update(std.mem.asBytes(&self.is_positive));
        }
    };

    pub fn toKey(self: LookaheadState) Allocator.Error!Key {
        return .{
            .base = try self.base.toKey(),
            .is_positive = self.is_positive,
        };
    }

    const BranchResult = struct {
        fail_sets: std.ArrayList(AcceptanceSet),
        acc_sets: std.ArrayList(AcceptanceSet),
    };

    pub fn getBranches(self: *LookaheadState) !BranchResult {
        const base_branches = try self.base.getBranches();
        defer base_branches.deinit();

        var base_fail = std.ArrayList(AcceptanceSet)
            .init(self.base.sub_states.allocator);
        try base_fail.append(base_branches.fail_set);
        var base_acc = std.ArrayList(AcceptanceSet)
            .init(self.base.sub_states.allocator);
        for (base_branches.prongs.items) |prong| {
            try base_acc.append(prong.set);
        }

        return if (self.is_positive) .{
            .fail_sets = base_fail,
            .acc_sets = base_acc,
        } else .{
            .fail_sets = base_acc,
            .acc_sets = base_fail,
        };
    }

    pub fn clone(self: LookaheadState) !LookaheadState {
        return .{
            .base = try self.base.clone(),
            .is_positive = self.is_positive,
        };
    }

    pub fn goEps(self: *LookaheadState) Allocator.Error!void {
        try self.base.goEps();
    }

    pub fn splitOn(self: LookaheadState, set: AcceptanceSet) !?LookaheadState {
        _ = self;
        _ = set;
    }

    pub fn deinit(self: LookaheadState) void {
        self.base.deinit();
    }
};

const ExecState = struct {
    blocks: std.ArrayList(*ir.Block),
    lookahead: std.ArrayList(LookaheadState),
    last_action: usize,
    instr: usize,
    instr_sub_idx: usize,
    had_fill: bool,

    pub fn init(allocator: Allocator, block: *ir.Block) !ExecState {
        var blocks = std.ArrayList(*ir.Block).init(allocator);
        try blocks.append(block);

        return .{
            .lookahead = std.ArrayList(LookaheadState).init(allocator),
            .instr_sub_idx = 0,
            .last_action = 0,
            .had_fill = false,
            .blocks = blocks,
            .instr = 0,
        };
    }

    pub fn splitOn(self: *const ExecState, set: AcceptanceSet) !?ExecState {
        if (self.blocks.items.len == 0) return null;
        const last = self.blocks.getLast();
        const instr = last.insts.items[self.instr];
        assert(instr.meta.isConsuming());
        assert(!set.isEmpty());

        switch (instr.tag) {
            // this only works because the set was built using add branches and
            // therefore cannot contain more characters in itself if it accepts the string
            // TODO: make this more robust
            .STRING => if (set.matchesChar(instr.data.str[self.instr_sub_idx])) {
                var new_state = try self.clone();
                if (self.instr_sub_idx + 1 == instr.data.str.len) {
                    new_state.instr += 1;
                    new_state.instr_sub_idx = 0;
                } else {
                    new_state.instr_sub_idx += 1;
                }

                return new_state;
            },
            .MATCH => for (instr.data.match.items) |prong| {
                var prong_set: AcceptanceSet = .{};
                for (prong.labels.items) |range| prong_set.addRange(range.from, range.to);
                if (!set.subSet(prong_set)) continue;

                var new_state = try self.clone();
                const blocks = new_state.blocks.items;
                blocks[blocks.len - 1] = prong.dest;
                new_state.instr = 0;

                return new_state;
            },
            else => unreachable,
        }

        return null;
    }

    pub fn defaultPass(self: *const ExecState) ?usize {
        return if (self.blocks.items.len == 0) self.last_action else null;
    }

    pub fn addBranches(
        self: *ExecState,
        list: *std.ArrayList(SplitBranch),
    ) !void {
        const blocks = self.blocks.items;
        if (blocks.len == 0) {
            // passing state
            try list.append(SplitBranch.initMatchAll(self.last_action));
            return;
        }
        const last = blocks[blocks.len - 1];
        const instr = last.insts.items[self.instr];
        assert(instr.meta.isConsuming());

        switch (instr.tag) {
            .STRING => try list.append(SplitBranch
                .initChar(instr.data.str[self.instr_sub_idx])),
            .MATCH => for (instr.data.match.items) |prong| {
                try list.append(SplitBranch.initProng(prong));
            },
            else => unreachable,
        }
    }

    pub fn canFillBranches(self: *ExecState) bool {
        if (self.blocks.items.len == 0) return false;
        const last = self.blocks.getLast();
        return !self.had_fill and
            last.meta.is_target and
            self.instr == 0 and
            self.instr_sub_idx == 0 and
            last.fail != null and
            last.fail.?.fail != null;
    }

    pub fn lookaheadGoEps(self: *ExecState) !void {
        for (self.lookahead.items) |*lookahead| {
            try lookahead.goEps();
        }
    }

    /// generates one exec state per branch
    pub fn fillBranches(self: *ExecState) !?ExecState {
        if (self.blocks.items.len == 0) return null;
        self.had_fill = true;
        const last = self.blocks.getLast();
        const next = last.fail.?;

        // ignore the fail block
        if (next.fail == null) return null;

        const output = try self.clone();
        const blocks = self.blocks.items;
        blocks[blocks.len - 1] = next;

        return output;
    }

    pub fn execJumps(self: *ExecState) !bool {
        if (self.blocks.items.len == 0) return false;
        const last = self.blocks.getLast();
        const blocks = self.blocks.items;
        const instr = last.insts.items[self.instr];

        assert(instr.meta.isConsuming());
        switch (instr.tag) {
            .JMP => {
                // the jump has no context
                const next = instr.data.jmp;
                blocks[blocks.len - 1] = next;
            },
            .NONTERM => {
                // the jump has context
                const jmp = instr.data.ctx_jmp;
                blocks[blocks.len - 1] = jmp.returns;
                try self.blocks.append(jmp.next);
                self.had_fill = false;
            },
            .RET, .EXIT_PASS => {
                // the jump has context
                self.had_fill = false;
                _ = self.blocks.pop();
                self.last_action = instr.data.action;
            },
            // this would be a full fail
            // should be unreachable as fill branches ignores the fail branch
            .FAIL, .EXIT_FAIL => unreachable,
            else => return false,
        }

        self.instr = 0;

        return true;
    }

    fn clone(self: ExecState) !ExecState {
        var lookahead = std.ArrayList(LookaheadState)
            .init(self.blocks.allocator);

        for (self.lookahead.items) |state| {
            try lookahead.append(try state.clone());
        }

        return .{
            .instr_sub_idx = self.instr_sub_idx,
            .blocks = try self.blocks.clone(),
            .had_fill = self.had_fill,
            .lookahead = lookahead,
            .instr = self.instr,
            .last_action = 0,
        };
    }

    pub const Key = struct {
        instr: usize,
        instr_sub_idx: usize,
        block: *ir.Block,
        lookahead: []LookaheadState.Key,

        pub fn eql(self: Key, other: Key) bool {
            for (self.lookahead, other.lookahead) |*s, *o| {
                if (!s.eql(o)) return false;
            }

            return self.instr == other.instr and
                self.instr_sub_idx == other.instr_sub_idx and
                self.block == other.block;
        }

        pub fn hash(self: Key, hasher: anytype) void {
            for (self.lookahead) |look| {
                look.hash(hasher);
            }

            hasher.update(std.mem.asBytes(&self.instr));
            hasher.update(std.mem.asBytes(&self.instr_sub_idx));
            hasher.update(std.mem.asBytes(&@intFromPtr(self.block)));
        }

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("({d}, {d}, {d})", .{
                self.block.id,
                self.instr,
                self.instr_sub_idx,
            });
        }
    };

    pub fn eql(self: ExecState, other: ExecState) bool {
        return self.blocks.getLastOrNull() == other.blocks.getLastOrNull() and
            self.instr == other.instr and
            self.instr_sub_idx == other.instr_sub_idx;
    }

    pub fn toKey(self: ExecState) !?Key {
        if (self.blocks.items.len == 0) return null;

        const lookahead = try self.blocks.allocator.alloc(
            LookaheadState.Key,
            self.lookahead.items.len,
        );
        for (lookahead, self.lookahead.items) |*to, from| to.* = try from.toKey();

        return .{
            .instr_sub_idx = self.instr_sub_idx,
            .block = self.blocks.getLast(),
            .lookahead = lookahead,
            .instr = self.instr,
        };
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.blocks.items.len == 0) {
            try writer.print("∅ ", .{});
        } else {
            try writer.print("({d}, {d}, {d})", .{
                self.blocks.getLast().id,
                self.instr,
                self.instr_sub_idx,
            });
        }
    }

    pub fn deinit(self: ExecState) void {
        self.blocks.deinit();
    }
};

const SplitBranch = struct {
    set: AcceptanceSet,
    final_action: ?usize,

    pub fn init(final_action: ?usize) SplitBranch {
        return .{
            .set = .{},
            .final_action = final_action,
        };
    }

    pub fn initMatchAll(final_action: ?usize) SplitBranch {
        var self = init(final_action);
        self.set.invert();
        return self;
    }

    pub fn initProng(prong: ir.MatchProng) SplitBranch {
        assert(prong.consuming);
        var self = init(null);

        for (prong.labels.items) |range| {
            self.set.addRange(range.from, range.to);
        }

        return self;
    }

    pub fn initChar(char: u8) SplitBranch {
        var self = init(null);
        self.set.addChar(char);
        return self;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("(branch: {s}{s})", .{
            self.set,
            if (self.final_action != null) "; (acc)" else "",
        });
    }
};

pub fn DfaCtx(comptime Hasher: type) type {
    return struct {
        pub fn eql(_: @This(), pseudo: DfaState.Key, key: DfaState.Key) bool {
            return pseudo.eql(key);
        }

        pub fn hash(_: @This(), key: DfaState.Key) u64 {
            var hasher = Hasher.init(0);
            key.hash(&hasher);
            return hasher.final();
        }
    };
}
