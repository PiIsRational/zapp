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
const ir = @import("low_ir.zig");
const ra = @import("rule_analyzer.zig");

const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const LookaheadType = enum {
    POSITIVE,
    NEGATIVE,
    NONE,
};

pub fn ListIter(comptime T: type) type {
    return struct {
        list: *std.ArrayList(T),
        index: usize = 0,

        pub fn init(list: *std.ArrayList(T)) @This() {
            return .{ .list = list };
        }

        pub fn next(self: *@This()) ?*T {
            if (self.list.items.len == self.index) return null;
            const item = &self.list.items[self.index];
            self.index += 1;

            return item;
        }

        pub fn getLast(self: @This()) *T {
            const items = self.list.items;
            return &items[items.len - 1];
        }

        pub fn reset(self: @This()) void {
            self.index = 0;
        }
    };
}

pub const LookaheadEmitter = struct {
    const Ctx = LookaheadCtx(std.hash.Wyhash);
    pub const LookaheadMap = std.HashMap(
        LookaheadState.Key,
        *ir.Block,
        Ctx,
        std.hash_map.default_max_load_percentage,
    );

    states: std.ArrayList(LookaheadState),
    map: LookaheadMap,
    map_keys: std.ArrayList(LookaheadState.Key),
    allocator: Allocator,
    nfa: ra.Automaton = undefined, // only set at emit

    pub fn init(allocator: Allocator) LookaheadEmitter {
        return .{
            .allocator = allocator,
            .map = LookaheadMap.init(allocator),
            .states = std.ArrayList(LookaheadState).init(allocator),
            .map_keys = std.ArrayList(LookaheadState.Key).init(allocator),
        };
    }

    /// goal:
    /// generate an nfa:
    /// the difference between an nfa and a dfa is epsilon transitions
    /// as having multiple possibilities to match a char can be replaced by epsilon transitions
    ///
    ///
    /// the spolution is to use the backtracking feature and normal jumps in blocks
    /// this solution would not be executable, but it would be understandable
    /// for the dfa generator
    pub fn emit(self: *LookaheadEmitter, start: *ir.Block) !ra.Automaton {
        self.nfa = ra.Automaton.init(self.allocator);
        const start_block = try self.nfa.getNew();
        const start_state = try LookaheadState.blockInit(self.allocator, start);
        try self.put((try start_state.toKey()).?, start_block);
        self.nfa.start = start_block;

        // the second state of the nfa is always the failing state (similar to dfa)
        const fail_block = try self.nfa.getNew();
        try fail_block.insts.append(ir.Instr.initTag(.FAIL));
        self.nfa.fail = fail_block;

        while (self.states.popOrNull()) |popped_state| {
            var state = popped_state;
            defer state.deinit();
            if (!try self.addEpsilons(&state)) continue;

            const key = try state.toKey();
            defer key.deinit();

            const block = self.map.get(key).?;
            if (block.insts.items.len != 0) continue;

            const branches = try state.getBranches();
            defer branches.deinit();

            try block.insts.append(ir.Instr.initTag(.MATCH));
            const instr = &block.insts.items[0];
            instr.data = .{ .match = std.ArrayList(ir.MatchProng).init(self.allocator) };

            if (!branches.fail_set.isEmpty()) {
                block.fail = fail_block;
            }

            for (self.branches.items) |branch| {
                var new_state = try state.splitOn(branch);
                while (try new_state.execJumps()) {}

                const new_key = try new_state.getKey();
                defer new_key.deinit();
                var labels = std.ArrayList(ir.Range).init(self.allocator);
                var iter = branch.set.rangeIter();
                while (iter.next()) |range| try labels.append(range);

                const new_block = self.map.get(new_key) orelse blk: {
                    const dst_block = try self.nfa.getNew();

                    if (new_state.isEmpty() == .ACCEPT) {
                        const action = new_state.base.action.?;
                        var pass_instr = ir.Instr.initTag(.RET);
                        pass_instr.data = .{ .action = action };
                        try dst_block.insts.append(pass_instr);
                    }

                    try self.put(try new_key.clone(self.allocator), dst_block);
                    break :blk dst_block;
                };

                try instr.data.match.append(.{
                    .labels = labels,
                    .dest = new_block,
                    .consuming = branch.final_action == null,
                });

                if (new_block.insts.items.len == 0) {
                    try self.states.append(new_state);
                } else {
                    new_state.deinit();
                }
            }
        }

        return self.nfa;
    }

    /// this method adds epsilon tansitions from `base` to all next states
    /// reachable through fillBranches
    /// the epsilon transitions are basically implemented using JMP.
    /// to get "choice" between the epsilons it makes use of backtracking.
    ///
    /// the method returns true iff it was able to add epsilons for `base`
    fn addEpsilons(self: *LookaheadEmitter, base: *LookaheadState) !bool {
        if (!base.canFillBranches()) return false;
        const base_key = (try base.toKey()).?;
        var first_block = self.map.get(base_key).?;

        const start = self.states.items.len;
        while (try base.fillBranches()) |pop_state| {
            var state = pop_state;

            while (try state.execJumps()) {}
            try self.states.append(state);
        }

        var last_transition = self.nfa.fail;
        for (self.states.items[start + 1 ..]) |state| {
            const new_block = try self.nfa.getNew();
            try self.put(try state.toKey(), new_block);

            var new_transition = try self.nfa.getNew();
            new_transition.fail = last_transition;
            try new_transition.insts.append(ir.Instr.initJmp(new_block));

            last_transition = new_transition;
        }

        // execute the loop body for `first_block`
        const state = self.states.items[start];
        const new_block = try self.nfa.getNew();
        try self.put(try state.toKey(), new_block);

        first_block.fail = last_transition;
        try first_block.insts.append(ir.Instr.initJmp(new_block));
        first_block.meta.is_target = true;

        return true;
    }

    pub fn put(self: *LookaheadEmitter, key: LookaheadState.Key, value: *ir.Block) !void {
        try self.map_keys.append(key);
        try self.map.putNoClobber(key, value);
    }

    pub fn deinit(self: *LookaheadEmitter) void {
        for (self.states.items) |state| state.deinit();
        for (self.map_keys.items) |key| key.deinit(self.allocator);

        self.map_keys.deinit();
        self.states.deinit();
        self.map.deinit();
    }
};

pub const LookaheadState = struct {
    base: ra.ExecState,
    look: LookaheadType,
    start: ra.ExecStart,
    sub_states: std.ArrayList(LookaheadState),

    pub fn blockInit(allocator: Allocator, block: *ir.Block) !LookaheadState {
        const base = try ra.ExecState.init(allocator, block);
        var self = baseInit(base, allocator);
        while (try self.execJumps()) {}

        return self;
    }

    pub fn baseInit(base: ra.ExecState, allocator: Allocator) LookaheadState {
        return init(base, .NONE, .STANDARD, allocator);
    }

    pub fn init(
        base: ra.Execstate,
        look: LookaheadType,
        start: ra.ExecStart,
        allocator: Allocator,
    ) LookaheadState {
        return .{
            .base = base,
            .look = look,
            .start = start,
            .sub_states = std.ArrayList(LookaheadState).init(allocator),
        };
    }

    pub fn canFillBranches(self: LookaheadState) bool {
        return self.base.canFillBranches();
    }

    pub fn fillBranches(self: *LookaheadState) !?LookaheadState {
        const result = try self.base.fillBranches();
        if (result == null) return null;

        return .{
            .look = self.look,
            .start = self.start,
            .base = result.?,
            .sub_states = try self.cloneSubStates(),
        };
    }

    pub fn execJumps(self: *LookaheadState) !bool {
        const result = try self.base.execJumps();
        if (result == .LOOKAHEAD) try self.splitOff();

        var had_change = result != .NO_CHANGE;
        for (self.sub_states.items) |*sub| {
            had_change = try sub.execJumps() or had_change;
        }

        return had_change;
    }

    pub fn isEmpty(self: *const LookaheadState) bool {
        _ = self;
        unreachable;
    }

    fn splitOff(self: *LookaheadState) !void {
        var new_branch = try self.clone();
        new_branch.setRoot();

        const instr = self.base.getCurrInstr() orelse unreachable;
        const blocks = self.base.blocks.items;
        const curr_blk = &blocks[blocks.len - 1];

        switch (instr.tag) {
            .NONTERM => {
                assert(!instr.meta.isConsuming());
                new_branch.look = if (instr.meta.pos) .POSITIVE else .NEGATIVE;

                // got to the next block
                curr_blk.* = instr.data.ctx_jmp.returns;
                self.base.instr = 0;
            },
            .STRING => {
                assert(!instr.meta.isConsuming());
                new_branch.look = if (instr.meta.pos) .POSITIVE else .NEGATIVE;

                // string match cannot be the last instruction of a block
                self.base.instr += 1;
            },
            .MATCH => {
                assert(instr.meta.isConsuming());
                new_branch.look = .POSITIVE;

                // got to the next block

                // there should be only one way
                // (this causes no problems because lowering only generates one)
                assert(instr.data.match.items.len == 1);
                curr_blk.* = instr.data.match.items[0].dest;
                self.base.instr = 0;
            },
            else => unreachable,
        }

        self.base.instr_sub_idx = 0;

        try self.sub_states.append(new_branch);
    }

    fn setRoot(self: *LookaheadState) void {
        assert(self.look != .NONE);
        assert(self.base.blocks.items.len > 0);

        self.base.setRoot();
        self.start = ra.ExecStart.init(
            self.base.blocks.items[0].id,
            self.base.instr,
        );
    }

    fn cloneSubStates(self: LookaheadState) Allocator.Error!std.ArrayList(LookaheadState) {
        var list = std.ArrayList(LookaheadState)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |sub| try list.append(try sub.clone());

        return list;
    }

    pub fn clone(self: LookaheadState) !LookaheadState {
        return .{
            .look = self.look,
            .start = self.start,
            .base = try self.base.clone(),
            .sub_states = try self.cloneSubStates(),
        };
    }

    pub const Key = struct {
        base: ra.ExecState.Key,
        look: LookaheadType,
        sub_states: []const LookaheadState.Key,

        pub fn eql(self: Key, other: Key) bool {
            for (self.sub_states, other.sub_states) |s, o| {
                if (!s.eql(o)) return false;
            }

            if (self.look != other.look) return false;
            if (self.base == null and other.base == null) return true;
            if (self.base != null and other.base == null or
                self.base == null and other.base != null) return false;

            return self.base.?.eql(other.base.?);
        }

        pub fn hash(self: Key, hasher: anytype) void {
            self.base.hash(hasher);

            assert(std.meta.hasUniqueRepresentation(LookaheadType));

            hasher.update(std.mem.asBytes(&self.look));
            for (self.sub_states) |sub| {
                assert(sub.look != .NONE); // sub states cannot have no lookahead
                sub.hash(hasher);
            }
        }

        pub fn deinit(self: Key, allocator: Allocator) void {
            for (self.sub_states) |sub| sub.deinit(allocator);
            allocator.free(self.sub_states);
        }
    };

    pub fn toKey(self: LookaheadState) !?Key {
        // TODO: make sure there is a canonical ordering for the states
        const base_key = self.base.toKey();
        if (base_key == null) {
            assert(self.isEmpty());
            return null;
        }

        var sub_states = std.ArrayList(LookaheadState.Key)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |from| {
            if (try from.toKey()) |from_key| {
                try sub_states.append(from_key);
            }
        }

        return .{
            .base = base_key.?,
            .look = self.look,
            .sub_states = try sub_states.toOwnedSlice(),
        };
    }

    pub fn deinit(self: LookaheadState) void {
        self.base.deinit();
        for (self.sub_states.items) |sub| sub.deinit();
        self.sub_states.deinit();
    }
};

pub fn LookaheadCtx(comptime Hasher: type) type {
    return struct {
        pub fn eql(_: @This(), pseudo: LookaheadState.Key, key: LookaheadState.Key) bool {
            return key.eql(pseudo);
        }

        pub fn hash(_: @This(), key: LookaheadState.Key) u64 {
            var hasher = Hasher.init(0);
            key.hash(hasher);
            return hasher.final();
        }
    };
}
