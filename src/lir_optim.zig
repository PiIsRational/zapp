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

const PassManager = @This();

ir: *ir.LowIr,

const PassError = Allocator.Error;

pub fn optimize(lir: *ir.LowIr) !void {
    const self: PassManager = .{
        .ir = lir,
    };

    var dfa_state = try DfaState.init(self.ir.allocator, self.ir.blocks.items[0]);
    defer dfa_state.deinit();

    std.debug.print("first variant\n\n", .{});
    for (dfa_state.sub_states.items, 0..) |state, i| {
        if (state.defaultPass()) |action| {
            std.debug.print(
                "state {d}:\naccepting!\nexecuting action {d}\n\n",
                .{ i, action },
            );
        } else {
            std.debug.print(
                "state {d}:\n{s}\n",
                .{ i, state.blocks.items[state.blocks.items.len - 1] },
            );
        }
    }

    const analysis = try dfa_state.getBranches();
    defer analysis.deinit();
    std.debug.print("fail_branch: {s}\n", .{analysis.fail_set});
    for (analysis.prongs.items) |*branch| _ = try dfa_state.splitOn(branch.set);
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
        try pass(self, blocks.*[i]);
    }
}

const MultiState = struct {
    const Context = DfaCtx(std.hash.Whyhash);
    pub const DfaMap = std.HashMap(
        *const DfaState.Key,
        ExecState.Key,
        Context,
        std.hash_map.default_max_load_percentage,
    );

    dfa_states: std.ArrayList(DfaState),
    map: DfaMap,
    map_keys: std.ArrayList(DfaState.Key),

    pub fn init(allocator: Allocator) MultiState {
        return .{
            .dfa_states = std.ArrayList(DfaState).init(allocator),
            .map = DfaMap.init(allocator),
            .map_keys = std.ArrayList(DfaState.Key).init(allocator),
        };
    }

    pub fn put(self: MultiState, key: DfaState.Key, value: ExecState.Key) !void {
        try self.map_keys.append(key);
        const keys = self.map_keys.items;
        const key_ptr = &keys[keys.len - 1];
        try self.map.put(key_ptr, value);
    }

    pub fn deinit(self: MultiState) void {
        for (self.dfa_states.items) |dfa_state| dfa_state.deinit();
        for (self.map_keys.items) |key| key.deinit();

        self.dfa_states.deinit();
        self.map_keys.deinit();
        self.map.deinit();
    }
};

const DfaState = struct {
    sub_states: std.ArrayList(ExecState),

    pub fn init(allocator: Allocator, block: *ir.Block) !DfaState {
        var sub_states = std.ArrayList(ExecState).init(allocator);
        try sub_states.append(try ExecState.init(allocator, block));

        var self: DfaState = .{
            .sub_states = sub_states,
        };

        try self.goEps();
        return self;
    }

    pub fn goEps(self: *DfaState) !void {
        while (try self.fillBranches() or try self.execJumps()) {}
        self.resetHadFill();
    }

    pub fn acceptingState(self: *const DfaState) bool {
        for (self.sub_states.items) |sub| {
            if (sub.defaultPass()) return true;
        }
        return false;
    }

    pub fn splitOn(self: *DfaState, set: AcceptanceSet) !DfaState {
        std.debug.print("split on {s}\n", .{set});
        var new_dfa_state: DfaState = .{
            .sub_states = std.ArrayList(ExecState)
                .init(self.sub_states.allocator),
        };

        for (self.sub_states.items) |sub| {
            if (try sub.splitOn(set)) |new_state| {
                try new_dfa_state.sub_states.append(new_state);
            }
        }

        std.debug.print("adding new state\n", .{});

        return new_dfa_state;
    }

    const BranchResult = struct {
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
            if (!prong.accept) continue;
            prong.set.cut(normal_match);
            if (prong.set.isEmpty()) {
                _ = prongs.swapRemove(i);
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
            if (prong.accept) continue;
            normal_match.merge(prong.set);

            var j: usize = i;
            while (j < prongs.items.len) : (j += 1) {
                const other_prong = &prongs.items[j];

                if (other_prong.accept) continue;
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

    const Key = struct {
        sub_states: []const ExecState.Key,

        pub fn deinit(self: Key, allocator: Allocator) void {
            allocator.free(self.sub_states);
        }
    };

    pub fn toKey(self: DfaState) !Key {
        var sub_states = try self.sub_states.allocator
            .alloc(ExecState.Key, self.sub_states.items.len);

        for (self.sub_states, 0..) |state, i| {
            sub_states[i] = state.toKey();
        }

        return .{
            .sub_states = sub_states,
        };
    }

    pub fn deinit(self: DfaState) void {
        for (self.sub_states.items) |sub_state| sub_state.deinit();
        self.sub_states.deinit();
    }
};

const ExecState = struct {
    blocks: std.ArrayList(*ir.Block),
    last_action: usize,
    instr: usize,
    instr_sub_idx: usize,
    had_fill: bool,

    pub fn init(allocator: Allocator, block: *ir.Block) !ExecState {
        var blocks = std.ArrayList(*ir.Block).init(allocator);
        try blocks.append(block);

        return .{
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
                new_state.instr += 1;

                return new_state;
            },
            else => unreachable,
        }

        return null;
    }

    pub const Key = struct {
        instr: usize,
        instr_sub_idx: usize,
        block: *ir.Block,

        pub fn eql(self: Key, other: Key) bool {
            return self.instr == other.instr and
                self.instr_sub_idx == other.instr_sub_idx and
                self.block == other.block;
        }

        pub fn hash(self: *const Key, hasher: anytype) void {
            assert(std.meta.hasUniqueRepresentation(usize));
            assert(std.meta.hasUniqueRepresentation(@intFromPtr(*ir.Block)));

            hasher.update(std.mem.asBytes(&self.instr));
            hasher.update(std.mem.asBytes(&self.instr_sub_idx));
            hasher.update(std.mem.asBytes(&@intFromPtr(self.block)));
        }
    };

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
            try list.append(SplitBranch.initMatchAll(true));
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

        return true;
    }

    fn clone(self: ExecState) !ExecState {
        return .{
            .instr_sub_idx = self.instr_sub_idx,
            .blocks = try self.blocks.clone(),
            .had_fill = self.had_fill,
            .instr = self.instr,
            .last_action = 0,
        };
    }

    pub fn toKey(self: ExecState) Key {
        return .{
            .instr_sub_idx = self.instr_sub_idx,
            .block = self.blocks.getLast(),
            .instr = self.instr,
        };
    }

    pub fn deinit(self: ExecState) void {
        self.blocks.deinit();
    }
};

const SplitBranch = struct {
    set: AcceptanceSet,
    accept: bool,

    pub fn init(accept: bool) SplitBranch {
        return .{
            .set = .{},
            .accept = accept,
        };
    }

    pub fn initMatchAll(accept: bool) SplitBranch {
        var self = init(accept);
        self.set.invert();
        return self;
    }

    pub fn initProng(prong: ir.MatchProng) SplitBranch {
        assert(prong.consuming);
        var self = init(false);

        for (prong.labels.items) |range| {
            self.set.addRange(range.from, range.to);
        }

        return self;
    }

    pub fn initChar(char: u8) SplitBranch {
        var self = init(false);
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
            if (self.accept) "; (acc)" else "",
        });
    }
};

pub fn DfaCtx(comptime Hasher: type) type {
    return struct {
        pub fn eql(_: @This(), pseudo: *const DfaState.Key, key: *const DfaState.Key) bool {
            for (key.sub_states.items, pseudo.sub_states.items) |key_sub, pseudo_sub| {
                if (!key_sub.eql(pseudo_sub)) return false;
            }

            return true;
        }

        pub fn hash(_: @This(), key: *const DfaState.Key) u64 {
            var hasher = Hasher.init(0);
            for (key.sub_states.items) |sub_state| {
                sub_state.hash(hasher);
            }

            return hasher.final();
        }
    };
}
