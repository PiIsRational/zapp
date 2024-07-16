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

const LookaheadEmitter = @This();
const LookaheadType = enum {
    POSITIVE,
    NEGATIVE,

    pub fn less(self: LookaheadType, other: LookaheadType) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }
};

const LookaheadMap = std.HashMap(
    LookaheadTopState.Key,
    *ir.Block,
    LookaheadCtx(std.hash.Wyhash),
    std.hash_map.default_max_load_percentage,
);

states: std.ArrayList(LookaheadTopState),
map: LookaheadMap,
map_keys: std.ArrayList(LookaheadTopState.Key),
allocator: Allocator,
nfa: ra.Automaton = undefined, // only set at emit

pub fn init(allocator: Allocator) LookaheadEmitter {
    return .{
        .allocator = allocator,
        .map = LookaheadMap.init(allocator),
        .states = std.ArrayList(LookaheadTopState).init(allocator),
        .map_keys = std.ArrayList(LookaheadTopState.Key).init(allocator),
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
    var start_state = try LookaheadTopState.blockInit(self.allocator, start);
    try self.put(try start_state.toKey(), start_block);
    try self.states.append(start_state);
    self.nfa.start = start_block;

    // the second state of the nfa is always the failing state (similar to dfa)
    const fail_block = try self.nfa.getNew();
    try fail_block.insts.append(ir.Instr.initTag(.FAIL));
    self.nfa.fail = fail_block;

    while (self.states.popOrNull()) |popped_state| {
        var state = popped_state;

        const key = try state.toKey();
        defer key.deinit(self.allocator);

        const block = self.map.get(key).?;

        if (block.insts.items.len != 0) {
            state.deinit();
            continue;
        }

        if (try state.canFillLookBranches()) {
            while (try state.fillLookBranches()) {}
            const new_key = try state.toKey();
            defer new_key.deinit(self.allocator);

            try self.states.append(state);
            assert(!new_key.eql(key));

            const blk = try self.getBlockForState(state, new_key);

            try block.insts.append(ir.Instr.initJmp(blk));
            continue;
        }

        if (try self.addEpsilons(&state)) {
            try self.states.append(state);
            continue;
        }

        if (try state.execJumps()) {
            if (try state.isLookFail()) {
                try block.insts.append(ir.Instr.initJmp(self.nfa.fail));
                state.deinit();
                continue;
            }

            const new_key = try state.toKey();
            defer new_key.deinit(self.allocator);

            try self.states.append(state);
            if (new_key.eql(key)) continue;

            const blk = try self.getBlockForState(state, new_key);

            try block.insts.append(ir.Instr.initJmp(blk));
            continue;
        }

        defer state.deinit();

        const branches = try state.getBranches();
        defer branches.deinit();

        try block.insts.append(ir.Instr.initTag(.MATCH));
        const instr = &block.insts.items[0];
        instr.data = .{ .match = std.ArrayList(ir.MatchProng).init(self.allocator) };

        if (block.fail == null and !branches.fail_set.isEmpty()) {
            block.fail = fail_block;
        }

        for (branches.prongs.items) |branch| {
            var new_state = try state.splitOn(branch);
            const new_key = try new_state.toKey();
            defer new_key.deinit(self.allocator);
            var labels = std.ArrayList(ir.Range).init(self.allocator);
            var iter = branch.set.rangeIter();
            while (iter.next()) |range| try labels.append(range);

            const new_block = try self.getBlockForState(new_state, new_key);
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

    try self.removeFails(&self.nfa);
    return self.nfa;
}

fn getBlockForState(
    self: *LookaheadEmitter,
    state: LookaheadTopState,
    key: LookaheadTopState.Key,
) !*ir.Block {
    return self.map.get(key) orelse blk: {
        const dst_block = try self.nfa.getNew();

        if (state.isEmpty()) {
            const action = state.action.?;
            var pass_instr = ir.Instr.initTag(.RET);
            pass_instr.data = .{ .action = action };
            try dst_block.insts.append(pass_instr);
        }

        try self.put(try key.clone(self.allocator), dst_block);
        break :blk dst_block;
    };
}

/// this method adds epsilon tansitions from `base` to all next states
/// reachable through fillBranches
/// the epsilon transitions are basically implemented using JMP.
/// to get "choice" between the epsilons it makes use of backtracking.
///
/// the method returns true iff it was able to add epsilons for `base`
fn addEpsilons(self: *LookaheadEmitter, base: *LookaheadTopState) !bool {
    if (!base.canFillBranches()) return false;

    const base_key = try base.toKey();
    defer base_key.deinit(self.allocator);
    const first_block = self.map.get(base_key).?;
    const start = self.states.items.len;
    const first_state = (try base.fillBranches()).?;

    while (try base.fillBranches()) |state| {
        try self.states.append(state);
    }

    try self.states.append(base.*);
    base.* = first_state;

    var last_transition = self.nfa.fail;
    for (self.states.items[start..]) |*state| {
        const key = try state.toKey();
        defer key.deinit(self.allocator);

        const new_block = try self.getBlockForState(state.*, key);

        var new_transition = try self.nfa.getNew();
        new_transition.fail = last_transition;
        try new_transition.insts.append(ir.Instr.initJmp(new_block));

        last_transition = new_transition;
    }

    // execute the loop body for `first_block`
    // the block is somewhat special as it does not contain a jmp instruction
    // pretty certain that this should not cause any problems, but not 100 %
    first_block.fail = last_transition;
    first_block.meta.is_target = true;

    return true;
}

pub fn put(self: *LookaheadEmitter, key: LookaheadTopState.Key, value: *ir.Block) !void {
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

fn removeFails(self: LookaheadEmitter, nfa: *ra.Automaton) !void {
    const markers = try self.allocator.alloc(bool, nfa.blocks.items.len);
    defer self.allocator.free(markers);
    @memset(markers, false);

    for (nfa.blocks.items) |state| {
        const instr = state.insts.items[0];
        if (instr.tag == .MATCH and instr.data.match.items.len == 0) {
            markers[state.id] = true;
        }

        if (instr.tag == .JMP and instr.data.jmp == nfa.fail) {
            markers[state.id] = true;
        }
    }

    for (0..nfa.blocks.items.len) |_| {
        var change = false;

        for (nfa.blocks.items) |state| {
            if (markers[state.id]) continue;

            if (state.fail != null and markers[state.fail.?.id]) {
                state.fail = state.fail.?.fail;
            }

            const instr = &state.insts.items[0];
            switch (instr.tag) {
                .MATCH => {
                    var new_prongs = std.ArrayList(ir.MatchProng).init(self.allocator);
                    for (instr.data.match.items) |*prong| {
                        if (!markers[prong.dest.id]) {
                            try new_prongs.append(prong.*);
                            continue;
                        }

                        if (prong.dest.meta.is_target) {
                            if (prong.dest.fail != nfa.fail) {
                                prong.dest = prong.dest.fail.?;
                                try new_prongs.append(prong.*);
                            } else {
                                prong.deinit();
                            }

                            state.meta.is_target = true;
                        } else {
                            prong.deinit();
                        }
                    }

                    if (new_prongs.items.len == 0) {
                        markers[state.id] = true;
                        change = true;
                    }

                    instr.data.match.deinit();
                    instr.data.match = new_prongs;
                },
                .JMP => if (markers[instr.data.jmp.id]) {
                    if (instr.data.jmp.meta.is_target and
                        instr.data.jmp.fail != nfa.fail)
                    {
                        instr.data.jmp = instr.data.jmp.fail.?;
                        instr.data.jmp.meta.is_target = true;
                    } else {
                        markers[state.id] = true;
                    }

                    change = true;
                },
                else => continue,
            }
        }

        if (!change) break;
    }

    var curr: usize = 0;
    var i: usize = 0;
    while (i < nfa.blocks.items.len) : (i += 1) {
        const s = nfa.blocks.items[i];
        if (markers[s.id]) {
            s.deinit(self.allocator);
            continue;
        }

        s.id = curr;
        nfa.blocks.items[curr] = s;
        curr += 1;
    }

    nfa.blocks.shrinkRetainingCapacity(curr);
}

fn addSubBranches(
    prongs: *std.ArrayList(ra.SplitBranch),
    base_buffer: *std.ArrayList(ra.SplitBranch),
    subs: []const LookaheadState,
) Allocator.Error!void {
    var sub_buffer = std.ArrayList(ra.SplitBranch)
        .init(base_buffer.allocator);
    defer sub_buffer.deinit();

    var base_no_consume = ra.AcceptanceSet.Full;
    for (base_buffer.items) |branch| {
        if (branch.final_action == null) base_no_consume.cut(branch.set);
    }

    for (subs) |sub| {
        try sub.addBranches(&sub_buffer);
        var index: usize = 0;
        for (sub_buffer.items) |*branch| {
            if (branch.final_action != null) {
                sub_buffer.items[index] = branch.*;
                index += 1;
                continue;
            }

            // do not append branches that would fail anyways
            branch.set.cut(base_no_consume);
            if (!branch.set.isEmpty()) try prongs.append(branch.*);
        }

        sub_buffer.shrinkRetainingCapacity(index);

        // finally analyze the accepting branches
        for (sub_buffer.items) |sub_branch| for (base_buffer.items) |*base_branch| {
            switch (sub.look) {
                .POSITIVE => base_branch.set.intersect(sub_branch.set),
                .NEGATIVE => base_branch.set.cut(sub_branch.set),
            }
        };

        sub_buffer.clearRetainingCapacity();
    }
}

/// the goal of cleanup is to order and deduplicate lookahead states
fn cleanUp(sub_states: *std.ArrayList(LookaheadState)) void {
    for (sub_states.items) |*sub| {
        cleanUp(&sub.lookaheads);
    }

    const Ctx = struct {
        fn less(_: @This(), lhs: LookaheadState, rhs: LookaheadState) bool {
            const l_k = lhs.base.toKey();
            const r_k = rhs.base.toKey();

            if (l_k == null) return r_k != null;
            if (r_k == null) return false;

            if (l_k.?.lessThan(r_k.?)) return true;
            if (!l_k.?.eql(r_k.?)) return false;

            if (lhs.look.less(rhs.look)) return true;
            if (lhs.look != rhs.look) return false;

            if (lhs.start.less(rhs.start)) return true;
            if (!lhs.start.eql(rhs.start)) return false;

            return false;
        }
    };

    // sort the sub_states
    std.sort.pdq(LookaheadState, sub_states.items, Ctx{}, Ctx.less);

    //remove doubled sub_states
    if (sub_states.items.len <= 1) return;

    var i: usize = 1;
    var idx: usize = 1;
    while (i < sub_states.items.len) : (i += 1) {
        const last = sub_states.items[idx - 1];
        const curr = &sub_states.items[i];

        if (last.eql(curr.*)) {
            curr.deinit();
            continue;
        }

        sub_states.items[idx] = curr.*;
        idx += 1;
    }

    sub_states.shrinkRetainingCapacity(idx);
}

const LookaheadTopState = struct {
    base: ra.ExecState,
    action: ?usize,
    sub_states: std.ArrayList(LookaheadState),

    fn isLookFail(self: LookaheadTopState) !bool {
        for (self.sub_states.items) |sub| {
            if (try sub.isDone(true) and sub.look == .NEGATIVE) return true;
        }

        return false;
    }

    fn getBranches(self: LookaheadTopState) !ra.BranchResult {
        var prongs = std.ArrayList(ra.SplitBranch)
            .init(self.sub_states.allocator);

        try self.addBranches(&prongs);
        var normal_match = try ra.canonicalizeBranches(&prongs);
        normal_match.invert();

        return .{
            .fail_set = normal_match,
            .prongs = prongs,
        };
    }

    fn addBranches(
        self: LookaheadTopState,
        prongs: *std.ArrayList(ra.SplitBranch),
    ) !void {
        const accepting = self.action != null;

        var base_buffer = std.ArrayList(ra.SplitBranch)
            .init(self.sub_states.allocator);
        defer base_buffer.deinit();

        if (!accepting) {
            try self.base.addBranches(&base_buffer, false);
        } else {
            try base_buffer.append(.{
                .set = ra.AcceptanceSet.Full,
                .final_action = self.action,
            });
        }

        try addSubBranches(prongs, &base_buffer, self.sub_states.items);

        for (base_buffer.items) |branch| {
            if (branch.set.isEmpty()) continue;
            try prongs.append(branch);
        }
    }

    fn splitOn(self: LookaheadTopState, branch: ra.SplitBranch) !LookaheadTopState {
        if (self.action) |action| return .{
            .base = try self.base.clone(),
            .action = action,
            .sub_states = std.ArrayList(LookaheadState).init(self.sub_states.allocator),
        };

        var new: LookaheadTopState = .{
            .base = try self.base.splitOn(branch.set) orelse undefined,
            .action = branch.final_action,
            .sub_states = std.ArrayList(LookaheadState).init(self.sub_states.allocator),
        };

        for (self.sub_states.items) |sub| {
            if (try sub.splitOn(branch)) |split| {
                try new.sub_states.append(split);
            }
        }

        return new;
    }

    fn blockInit(allocator: Allocator, block: *ir.Block) !LookaheadTopState {
        const base = try ra.ExecState.init(allocator, block);
        return baseInit(base, allocator);
    }

    fn baseInit(base: ra.ExecState, allocator: Allocator) LookaheadTopState {
        return LookaheadTopState.init(base, allocator);
    }

    fn init(
        base: ra.ExecState,
        allocator: Allocator,
    ) LookaheadTopState {
        return .{
            .action = null,
            .base = base,
            .sub_states = std.ArrayList(LookaheadState).init(allocator),
        };
    }

    fn canFillBranches(self: LookaheadTopState) bool {
        if (self.action != null) return false;
        return self.base.canFillBranches();
    }

    fn canFillLookBranches(self: LookaheadTopState) !bool {
        for (self.sub_states.items) |sub| {
            if (try sub.canFillBranches()) return true;
            if (try sub.canFillSubBranches()) return true;
        }

        return false;
    }

    fn eql(self: LookaheadTopState, other: LookaheadTopState) bool {
        if ((self.action == null and other.action == null and
            !self.base.eql(other.base) or self.action != other.action) or
            self.look != other.look or !self.start.eql(other.start) or
            self.sub_states.items.len != other.sub_states.items.len)
        {
            return false;
        }

        for (self.sub_states.items, other.sub_states.items) |s, o| {
            if (!s.eql(o)) return false;
        }

        return true;
    }

    fn fillLookBranches(self: *LookaheadTopState) !bool {
        var buf = std.ArrayList(LookaheadState)
            .init(self.sub_states.allocator);
        defer buf.deinit();

        var result = false;

        for (self.sub_states.items) |*state| {
            result = try state.fillSubBranches() or result;
            if (!state.base.canFillBranches()) continue;
            result = true;

            while (try state.fillBranches()) |branched| {
                try buf.append(branched);
            }
        }

        try self.sub_states.appendSlice(buf.items);
        return result;
    }

    fn fillBranches(self: *LookaheadTopState) !?LookaheadTopState {
        const result = try self.base.fillBranches() orelse return null;

        return .{
            .action = null,
            .base = result,
            .sub_states = try self.cloneSubStates(),
        };
    }

    fn execJumps(self: *LookaheadTopState) !bool {
        const result = try self.base.execJumps();
        var buf = std.ArrayList(LookaheadState).init(self.sub_states.allocator);
        defer buf.deinit();

        if (result == .LOOKAHEAD) {
            try self.sub_states.append(try self.splitOff());
            return true;
        }

        var had_change = result != .NO_CHANGE;
        for (self.sub_states.items) |*sub| {
            had_change = try sub.execJumps() or had_change;
        }
        try self.sub_states.appendSlice(buf.items);

        if (self.base.blocks.items.len == 0) {
            self.action = self.base.last_action;
        }

        return had_change;
    }

    /// the state is accepting
    fn isEmpty(self: *const LookaheadTopState) bool {
        return self.action != null and self.sub_states.items.len == 0;
    }

    fn splitOff(self: *LookaheadTopState) !LookaheadState {
        var new_branch: LookaheadState = .{
            .lookaheads = std.ArrayList(LookaheadState).init(self.sub_states.allocator),
            .base = try self.base.clone(),
            .look = undefined,
            .start = undefined,
        };

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
                assert(!instr.meta.isConsuming());
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

        new_branch.setRoot();
        return new_branch;
    }

    fn cloneSubStates(
        self: LookaheadTopState,
    ) Allocator.Error!std.ArrayList(LookaheadState) {
        var list = std.ArrayList(LookaheadState)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |sub| try list.append(try sub.clone());

        return list;
    }

    fn clone(self: LookaheadTopState, deep: bool) !LookaheadTopState {
        return .{
            .action = self.action,
            .look = self.look,
            .start = self.start,
            .base = try self.base.clone(),
            .sub_states = if (deep)
                try self.cloneSubStates()
            else
                std.ArrayList(LookaheadState).init(self.sub_states.allocator),
        };
    }

    const Key = struct {
        base: ra.ExecState.Key,
        action: ?usize,
        sub_states: []const LookaheadState.Key,

        fn eql(self: Key, other: Key) bool {
            if (self.sub_states.len != other.sub_states.len) return false;
            for (self.sub_states, other.sub_states) |s, o| {
                if (!s.eql(o)) return false;
            }

            if (self.action != other.action) return false;
            if (self.action != null) return true;

            return self.base.eql(other.base);
        }

        fn hash(self: Key, hasher: anytype) void {
            assert(std.meta.hasUniqueRepresentation(LookaheadType));
            if (self.action) |action| {
                hasher.update("1"); // action
                hasher.update(std.mem.asBytes(&action));
            } else {
                hasher.update("0"); // no action
                self.base.hash(hasher);
            }

            for (self.sub_states) |sub| {
                sub.hash(hasher);
            }
        }

        fn clone(self: Key, allocator: Allocator) !Key {
            const sub_states = try allocator.alloc(LookaheadState.Key, self.sub_states.len);
            for (self.sub_states, sub_states) |from, *to| to.* = try from.clone(allocator);

            return .{
                .base = self.base,
                .action = self.action,
                .sub_states = sub_states,
            };
        }

        fn deinit(self: Key, allocator: Allocator) void {
            for (self.sub_states) |sub| {
                sub.deinit(allocator);
            }
            allocator.free(self.sub_states);
        }

        pub fn format(
            self: Key,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (self.action) |act| {
                try writer.print("(act: {d})", .{act});
            } else {
                try writer.print("{s} {{", .{self.base});
                for (self.sub_states) |sub| {
                    try writer.print("{s}, ", .{sub});
                }
                try writer.print("}}", .{});
            }
        }
    };

    fn toKey(self: *LookaheadTopState) !Key {
        cleanUp(&self.sub_states);

        // here undefined makes sense as accept is first inspected
        const base_key = if (self.action == null)
            self.base.toKey() orelse undefined
        else
            undefined;

        var sub_states = std.ArrayList(LookaheadState.Key).init(self.sub_states.allocator);
        for (self.sub_states.items) |*from| {
            if (try from.toKey(self.sub_states.allocator)) |key| {
                try sub_states.append(key);
            }
        }

        return .{
            .action = self.action,
            .base = base_key,
            .sub_states = try sub_states.toOwnedSlice(),
        };
    }

    fn deinit(self: LookaheadTopState) void {
        self.base.deinit();
        for (self.sub_states.items) |sub| sub.deinit();
        self.sub_states.deinit();
    }

    pub fn format(
        self: LookaheadTopState,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s} {{", .{self.base});
        for (self.sub_states.items) |sub| {
            try writer.print("{s}, ", .{sub});
        }
        try writer.print("}}", .{});
    }
};

const LookaheadState = struct {
    base: ra.ExecState,
    look: LookaheadType,
    start: ra.ExecPlace,
    lookaheads: std.ArrayList(LookaheadState),

    fn isDoneOnSet(self: LookaheadState, set: ra.AcceptanceSet) !bool {
        var next = try self.base.splitOn(set) orelse return false;
        defer next.deinit();
        var val: ra.ExecState.ExecJmpsResult = .CHANGE;
        while (val == .CHANGE) : (val = try next.execJumps()) {
            const blocks = next.blocks.items;

            if (blocks.len == 0 or
                blocks.len == 1 and
                (blocks[0].id != self.start.block_id or next.instr != self.start.instr))
            {
                return true;
            }
        }

        return false;
    }

    fn lookaheadOnStart(self: LookaheadState) bool {
        if (self.base.blocks.items.len == 0) return false;
        const block = self.base.blocks.items[0];
        return self.base.blocks.items.len == 1 and
            block.id == self.start.block_id and
            self.base.instr == self.start.instr;
    }

    fn splitOn(self: LookaheadState, branch: ra.SplitBranch) !?LookaheadState {
        var looks = std.ArrayList(LookaheadState).init(self.lookaheads.allocator);

        for (self.lookaheads.items) |look| {
            if (try look.splitOn(branch)) |l| {
                try looks.append(l);
            }
        }

        var new: LookaheadState = .{
            .base = if (try self.isDone(false))
                try self.base.clone()
            else
                try self.base.splitOn(branch.set) orelse {
                    for (looks.items) |l| l.deinit();
                    looks.deinit();
                    return null;
                },
            .look = self.look,
            .start = self.start,
            .lookaheads = looks,
        };

        if (try new.isDone(true)) {
            new.deinit();
            return null;
        }

        return new;
    }

    fn isDone(self: LookaheadState, deep: bool) !bool {
        const can_jump = self.base.canExecJumps();
        var copy = if (can_jump) try self.base.clone() else self.base;
        defer if (can_jump) copy.deinit();
        var val: ra.ExecState.ExecJmpsResult = .CHANGE;

        while (can_jump and val == .CHANGE) : (val = try copy.execJumps()) {
            const blocks = copy.blocks.items;

            if (blocks.len == 0 or blocks.len == 1 and
                (blocks[0].id != self.start.block_id or copy.instr != self.start.instr))
            {
                break;
            }
        }

        const blocks = copy.blocks.items;
        if (blocks.len != 0 and (blocks.len != 1 or
            blocks[0].id == self.start.block_id and
            copy.instr == self.start.instr))
        {
            return false;
        }

        if (deep) {
            for (self.lookaheads.items) |look| {
                if (!try look.isDone(true)) return false;
            }
        }

        return true;
    }

    fn canFillBranches(self: LookaheadState) !bool {
        if (try self.isDone(false)) return false;
        return self.base.canFillBranches();
    }

    fn eql(self: LookaheadState, other: LookaheadState) bool {
        if (!self.base.eql(other.base) or
            self.look != other.look or
            !self.start.eql(other.start) or
            self.lookaheads.items.len != other.lookaheads.items.len)
        {
            return false;
        }

        for (self.lookaheads.items, other.lookaheads.items) |s, o| {
            if (!s.eql(o)) return false;
        }

        return true;
    }

    fn canFillSubBranches(self: LookaheadState) !bool {
        for (self.lookaheads.items) |sub| {
            if (try sub.canFillBranches()) return true;
            if (try sub.canFillSubBranches()) return true;
        }

        return false;
    }

    fn fillSubBranches(self: *LookaheadState) !bool {
        var result = false;
        var i: usize = 0;
        while (i < self.lookaheads.items.len) : (i += 1) {
            var look = self.lookaheads.items[i];
            result = try look.fillSubBranches() or result;
            if (!try look.canFillBranches()) continue;
            result = true;

            while (try look.fillBranches()) |new| {
                try self.lookaheads.append(new);
            }

            self.lookaheads.items[i] = look;
        }

        return result;
    }

    fn fillBranches(self: *LookaheadState) !?LookaheadState {
        const result = try self.base.fillBranches() orelse return null;

        return .{
            .look = self.look,
            .start = self.start,
            .base = result,
            .lookaheads = try self.cloneLookaheads(),
        };
    }

    fn execJumps(self: *LookaheadState) !bool {
        if (try self.isDone(false)) return try self.execJumpsChilds();

        const result = if (self.lookaheadOnStart())
            try self.base.execForceJumps()
        else
            try self.base.execJumps();

        if (result == .LOOKAHEAD) {
            const split_result = try self.splitOff();
            if (split_result) |new| {
                try self.lookaheads.append(new);
            }

            return split_result != null;
        }

        return try self.execJumpsChilds() or result == .CHANGE;
    }

    fn execJumpsChilds(self: *LookaheadState) Allocator.Error!bool {
        var change = false;
        for (self.lookaheads.items) |*sub| {
            change = try sub.execJumps() or change;
        }

        return change;
    }

    fn addBranches(
        self: LookaheadState,
        prongs: *std.ArrayList(ra.SplitBranch),
    ) !void {
        var base_buffer = std.ArrayList(ra.SplitBranch)
            .init(prongs.allocator);
        defer base_buffer.deinit();

        if (try self.isDone(false)) {
            try base_buffer.append(.{
                .set = ra.AcceptanceSet.Full,
                .final_action = 0,
            });
        } else {
            try self.base.addBranches(&base_buffer, false);

            for (base_buffer.items) |*branch| {
                if (!try self.isDoneOnSet(branch.set)) continue;
                branch.final_action = 0; // this is a terminal action
            }
        }

        try addSubBranches(prongs, &base_buffer, self.lookaheads.items);

        for (base_buffer.items) |branch| {
            if (branch.set.isEmpty()) continue;
            try prongs.append(branch);
        }
    }

    fn splitOff(self: *LookaheadState) !?LookaheadState {
        if (self.lookaheadOnStart()) return null;
        var new_branch = try self.clone();

        const instr = self.base.getCurrInstr() orelse unreachable;
        const blocks = self.base.blocks.items;
        const curr_blk = &blocks[blocks.len - 1];

        assert(!instr.meta.isConsuming());
        switch (instr.tag) {
            .NONTERM => {
                new_branch.look = if (instr.meta.pos) .POSITIVE else .NEGATIVE;
                // got to the next block
                curr_blk.* = instr.data.ctx_jmp.returns;
                self.base.instr = 0;
            },
            .STRING => {
                new_branch.look = if (instr.meta.pos) .POSITIVE else .NEGATIVE;
                // string match cannot be the last instruction of a block
                self.base.instr += 1;
            },
            .MATCH => {
                new_branch.look = .POSITIVE;
                // there should be only one way
                // (this causes no problems because lowering only generates one)
                assert(instr.data.match.items.len == 1);
                curr_blk.* = instr.data.match.items[0].dest;
                self.base.instr = 0;
            },
            else => unreachable,
        }

        self.base.instr_sub_idx = 0;

        new_branch.setRoot();
        return new_branch;
    }

    fn setRoot(self: *LookaheadState) void {
        assert(self.base.blocks.items.len > 0);

        self.base.setRoot();
        self.start = ra.ExecPlace.init(
            self.base.blocks.items[0].id,
            self.base.instr,
        );
    }

    fn clone(self: LookaheadState) Allocator.Error!LookaheadState {
        return .{
            .look = self.look,
            .start = self.start,
            .base = try self.base.clone(),
            .lookaheads = try self.cloneLookaheads(),
        };
    }

    fn cloneLookaheads(self: LookaheadState) !std.ArrayList(LookaheadState) {
        var looks = std.ArrayList(LookaheadState).init(self.lookaheads.allocator);
        for (self.lookaheads.items) |look| try looks.append(try look.clone());
        return looks;
    }

    const Key = struct {
        base: ?ra.ExecState.Key,
        look: LookaheadType,
        lookaheads: []const Key,

        fn eql(self: Key, other: Key) bool {
            if (self.look != other.look or
                self.lookaheads.len != other.lookaheads.len or
                self.base == null and other.base != null or
                self.base != null and other.base == null or
                !(self.base == null and other.base == null or
                self.base.?.eql(other.base.?))) return false;

            for (self.lookaheads, other.lookaheads) |s, o| {
                if (!s.eql(o)) return false;
            }

            return true;
        }

        fn hash(self: Key, hasher: anytype) void {
            assert(std.meta.hasUniqueRepresentation(LookaheadType));

            if (self.base) |base| {
                hasher.update("1");
                base.hash(hasher);
            } else {
                hasher.update("0");
            }

            hasher.update(std.mem.asBytes(&self.look));

            for (self.lookaheads) |look| {
                look.hash(hasher);
            }
        }

        fn clone(self: Key, allocator: Allocator) !Key {
            const looks = try allocator.alloc(Key, self.lookaheads.len);
            for (self.lookaheads, looks) |from, *to| to.* = try from.clone(allocator);

            return .{
                .lookaheads = looks,
                .base = self.base,
                .look = self.look,
            };
        }

        fn deinit(self: Key, allocator: Allocator) void {
            for (self.lookaheads) |look| {
                look.deinit(allocator);
            }
            allocator.free(self.lookaheads);
        }

        pub fn format(
            self: Key,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("{s}", .{if (self.look == .POSITIVE) "&" else "!"});
            if (self.base) |base| {
                try writer.print("{s} {{ ", .{base});
            } else {
                try writer.print("∅ ", .{});
            }

            for (self.lookaheads) |look| {
                try writer.print("{s}, ", .{look});
            }

            try writer.print("}}", .{});
        }
    };

    fn toKey(self: LookaheadState, allocator: Allocator) !?Key {
        const base_key = self.base.toKey();
        var looks = std.ArrayList(Key).init(allocator);
        for (self.lookaheads.items) |look| {
            const key = try look.toKey(allocator);
            if (key) |k| try looks.append(k);
        }

        if (looks.items.len == 0 and base_key == null) return null;

        return .{
            .lookaheads = try looks.toOwnedSlice(),
            .base = base_key,
            .look = self.look,
        };
    }

    fn deinit(self: LookaheadState) void {
        for (self.lookaheads.items) |look| look.deinit();
        self.lookaheads.deinit();
        self.base.deinit();
    }

    pub fn format(
        self: LookaheadState,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}", .{if (self.look == .POSITIVE) "&" else "!"});
        try writer.print("{s} {{ ", .{self.base});

        for (self.lookaheads.items) |look| {
            try writer.print("{s}, ", .{look});
        }

        try writer.print("}}", .{});
    }
};

fn LookaheadCtx(comptime Hasher: type) type {
    return struct {
        pub fn eql(
            _: @This(),
            pseudo: LookaheadTopState.Key,
            key: LookaheadTopState.Key,
        ) bool {
            return key.eql(pseudo);
        }

        pub fn hash(_: @This(), key: LookaheadTopState.Key) u64 {
            var hasher = Hasher.init(0);
            key.hash(&hasher);
            return hasher.final();
        }
    };
}
