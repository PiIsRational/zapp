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

        // the first state of the nfa is always the failing state
        const fail_block = try self.nfa.getNew();
        try fail_block.insts.append(ir.Instr.initTag(.FAIL));
        self.nfa.fail = fail_block;
        _ = start;

        self.fna.start = try self.epsStep();

        return self.nfa;
    }

    fn addEpsilons(self: *LookaheadEmitter, base: *LookaheadState) !bool {
        if (!base.canFillBranches()) return false;

        const start = self.states.items.len;
        var top_state = base;
        var first = true;

        while (try base.fillBranches()) |state| {
            if (first) top_state = state;
            first = false;

            try self.states.append(state);
        }

        var last_transition = self.nfa.fail;
        for (self.states.items[start..]) |state| {
            if (state == top_state) continue;
            var new_transition = try self.nfa.getNew();
            new_transition.fail = last_transition;

            const new_block = try self.nfa.getNew();
            try self.put(try state.toKey(), new_block);

            last_transition = new_transition;
        }

        var first_block = try self.nfa.getNew();
        first_block.fail = last_transition;

        try self.put(try top_state.toKey(), first_block);
        return first_block;
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

    pub fn baseInit(base: ra.ExecState) LookaheadState {
        return init(base, .NONE, .STANDARD);
    }

    pub fn init(
        base: ra.Execstate,
        look: LookaheadType,
    ) LookaheadState {
        return .{
            .base = base,
            .look = look,
        };
    }

    pub const Key = struct {
        base: ?ra.ExecState.Key,
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

    pub fn canFillBranches(self: LookaheadState) bool {
        return self.base.canFillBranches();
    }

    pub fn fillBranches(self: *LookaheadState) !?LookaheadState {
        const result = try self.base.fillBranches();
        if (result == null) return null;

        return .{
            .look = self.look,
            .lifetime = self.lifetime,
            .start = self.start,
            .base = result,
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

    fn splitOff(self: *LookaheadState) !void {
        var new_branch = try self.clone();
        new_branch.setRoot();

        const instr = self.base.getCurrInstr() orelse unreachable;
        const blocks = self.base.blocks.items;
        const curr_blk = &blocks[blocks.len - 1];

        switch (instr.tag) {
            .NONTERM => {
                assert(!instr.meta.isConsuming());
                new_branch.look = if (self.instr.meta.pos) .POSITIVE else .NEGATIVE;

                // got to the next block
                curr_blk.* = instr.data.ctx_jmp.returns;
                self.base.instr = 0;
            },
            .STRING => {
                assert(!instr.meta.isConsuming());
                new_branch.look = if (self.instr.meta.pos) .POSITIVE else .NEGATIVE;

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

        self.instr_sub_idx = 0;

        try self.branches.append(new_branch);
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

    fn cloneSubStates(self: LookaheadState) std.ArrayList(LookaheadState) {
        var list = std.ArrayList(LookaheadState)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |sub| try list.append(try sub.clone());

        return list;
    }

    pub fn clone(self: LookaheadState) !LookaheadState {
        return .{
            .look = self.look,
            .lifetime = self.lifetime,
            .start = self.start,
            .base = try self.base.clone(),
            .sub_states = try self.cloneSubStates(),
        };
    }

    pub fn toKey(self: LookaheadState) !?Key {
        const sub_states = try self.sub_states.allocator.alloc(
            LookaheadState.Key,
            self.sub_states.items.len,
        );
        for (sub_states, self.sub_states.items) |*to, from| to.* = try from.toKey();

        return .{
            .base = self.base.toKey(),
            .look = self.lookahead,
            .lifetime = self.lifetime,
            .sub_states = self.sub_states,
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
