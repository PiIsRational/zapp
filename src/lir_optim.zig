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
const ra = @import("rule_analyzer.zig");
const gvgen = @import("graphviz_gen.zig");
const look = @import("lookaheads.zig");

const PassManager = @This();

ir: *ir.LowIr,

const PassError = Allocator.Error;

// the plan would be to treat everything in two passes:
//
// * first try to remove lookahead and get a simple model for an nfa
// * second implement the nfa to dfa conversion (already implemented)
//
// it would be nice to have some minimization algorithms.
// there woule also be a benefit from refactoring some of the code to
// make all the analyses simpler and more reusable.

pub fn optimize(lir: *ir.LowIr) !void {
    const stdout = std.io.getStdOut().writer();
    const self: PassManager = .{
        .ir = lir,
    };

    var emitter = look.LookaheadEmitter.init(self.ir.allocator);
    defer emitter.deinit();

    const nfa = try emitter.emit(self.ir.blocks.items[0]);

    defer nfa.deinit(self.ir.allocator);
    try gvgen.genAutomaton(stdout, nfa, "nfa");

    //var dfa_gen = DfaGen.init(self.ir.allocator);
    //defer dfa_gen.deinit();

    //const dfa = try dfa_gen.gen(self.ir.blocks.items[0]);

    //try gvgen.genAutomaton(stdout, dfa, "dfa");
    //defer dfa.deinit(self.ir.allocator);
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

    pub fn gen(self: *DfaGen, start_block: *ir.Block) !ra.Automaton {
        var dfa = ra.Automaton.init(self.allocator);
        var start_state = try DfaState.init(self.allocator, start_block);
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
    sub_states: std.ArrayList(ra.ExecState),
    action: ?usize,

    pub fn init(allocator: Allocator, block: *ir.Block) !DfaState {
        return try initFromExec(allocator, try ra.ExecState.init(allocator, block));
    }

    pub fn initFromExec(allocator: Allocator, exec: ra.ExecState) !DfaState {
        var sub_states = std.ArrayList(ra.ExecState).init(allocator);
        try sub_states.append(exec);

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
        self.deduplicate();
        if (self.isEmpty()) {
            assert(self.sub_states.items.len > 0);
            self.action = self.sub_states.items[0].last_action;
        }
    }

    /// deduplicates the sub states of the dfa state
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

    /// reordering the sub states of the dfa state to get
    /// a canonical representation of dfa states for hashing purposes
    fn reorder(self: *DfaState) void {
        const Ctx = struct {
            pub fn less(_: @This(), lhs: ra.ExecState, rhs: ra.ExecState) bool {
                const l_k = lhs.toKey();
                const r_k = rhs.toKey();

                if (l_k == null) return r_k != null;
                if (r_k == null) return false;

                return l_k.?.lessThan(r_k.?);
            }
        };

        std.sort.pdq(ra.ExecState, self.sub_states.items, Ctx{}, Ctx.less);
    }

    pub fn clone(self: *const DfaState) !DfaState {
        var sub_states = std.ArrayList(ra.ExecState)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |sub_state| try sub_states.append(sub_state);

        return .{ .sub_states = sub_states, .action = self.action };
    }

    pub fn splitOn(self: *DfaState, branch: ra.SplitBranch) !DfaState {
        var new_dfa_state: DfaState = .{
            .sub_states = std.ArrayList(ra.ExecState)
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
        fail_set: ra.AcceptanceSet,
        prongs: std.ArrayList(ra.SplitBranch),

        pub fn deinit(self: BranchResult) void {
            self.prongs.deinit();
        }
    };

    pub fn getBranches(self: *DfaState) !BranchResult {
        var prongs = std.ArrayList(ra.SplitBranch)
            .init(self.sub_states.allocator);
        for (self.sub_states.items) |*state| {
            try state.addBranches(&prongs);
        }

        const full_match = try ra.canonicalizeBranches(&prongs);

        // next emit the block
        var fail_match = full_match;
        fail_match.invert();

        return .{
            .fail_set = fail_match,
            .prongs = prongs,
        };
    }

    fn resetHadFill(self: *DfaState) void {
        for (self.sub_states.items) |*sub| sub.had_fill = false;
    }

    fn execJumps(self: *DfaState) !bool {
        var had_change = false;
        for (self.sub_states.items) |*sub| {
            const jmp_result = try sub.execJumps();
            assert(jmp_result != .LOOKAHEAD);
            had_change = jmp_result == .CHANGE or had_change;
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
        sub_states: []const ra.ExecState.Key,
        action: ?usize,

        pub fn clone(self: Key, allocator: Allocator) !Key {
            const sub_states = try allocator.dupe(ra.ExecState.Key, self.sub_states);
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

    pub fn toKey(self: *DfaState) !Key {
        self.reorder();

        var sub_states = std.ArrayList(ra.ExecState.Key)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |*state| {
            if (state.toKey()) |key| {
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
