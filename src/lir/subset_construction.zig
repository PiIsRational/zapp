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

const DfaGen = @This();
const Context = DfaCtx(std.hash.Wyhash);
pub const DfaMap = std.HashMap(
    DfaState.Key,
    *ir.Block,
    Context,
    std.hash_map.default_max_load_percentage,
);

dfa: ra.Automaton,
dfa_states: std.ArrayList(DfaState),
map: DfaMap,
map_keys: std.ArrayList(DfaState.Key),
allocator: Allocator,
key_scratch: std.AutoHashMap(usize, usize),

pub fn init(allocator: Allocator) DfaGen {
    return .{
        .dfa = ra.Automaton.init(allocator),
        .dfa_states = std.ArrayList(DfaState).init(allocator),
        .map = DfaMap.init(allocator),
        .map_keys = std.ArrayList(DfaState.Key).init(allocator),
        .allocator = allocator,
        .key_scratch = std.AutoHashMap(usize, usize).init(allocator),
    };
}

/// generates a Dfa (automaton with only match and ret/fail)
pub fn genDfa(self: *DfaGen, start_block: *ir.Block) !ra.Automaton {
    var start_state = try DfaState.init(self.allocator, start_block);
    try self.dfa_states.append(start_state);
    try self.put(try start_state.toKey(&self.key_scratch), try self.dfa.getNew());

    const fail_block = try self.dfa.getNew();
    try fail_block.insts.append(ir.Instr.initTag(.TERM_FAIL));

    self.dfa.start = self.dfa.blocks.items[0];
    self.dfa.fail = fail_block;

    var new_states = std.ArrayList(DfaState).init(self.allocator);
    defer new_states.deinit();

    while (self.dfa_states.items.len != 0) {
        for (self.dfa_states.items) |*state| {
            defer state.deinit();
            const branches = try state.getBranches();
            defer branches.deinit();

            // get the block
            const curr_key = try state.toKey(&self.key_scratch);
            defer curr_key.deinit(self.allocator);

            const block = self.map.get(curr_key).?;
            const insts = block.insts.items;
            if (insts.len > 1 or insts.len == 1 and insts[0].tag != .PRE_ACCEPT) {
                continue;
            }

            try block.insts.append(ir.Instr.initTag(.MATCH));
            const instr = &block.insts.items[block.insts.items.len - 1];
            instr.data = .{ .match = std.ArrayList(ir.MatchProng)
                .init(self.allocator) };

            if (!branches.fail_set.isEmpty()) {
                block.fail = fail_block;
            }

            // add the prongs and get the destination block
            for (branches.prongs.items) |prong| {
                var split_state = try state.splitOn(prong);
                try split_state.goEps();

                var labels = std.ArrayList(ir.Range).init(self.allocator);
                var iter = prong.set.rangeIter();
                while (iter.next()) |range| try labels.append(range);

                const dst_blk = try self.getBlock(&split_state);

                try instr.data.match.append(.{
                    .labels = labels,
                    .dest = dst_blk,
                    .consuming = prong.final_action == null,
                });

                if (dst_blk.insts.items.len == 0 or
                    dst_blk.insts.items[0].tag == .PRE_ACCEPT)
                {
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

    try ra.mergeDest(self.allocator, &self.dfa);
    return self.dfa;
}

fn getBlock(self: *DfaGen, state: *DfaState) !*ir.Block {
    const semi_empty = state.isSemiEmpty();
    const empty = !semi_empty and state.isEmpty();
    if (semi_empty) state.removeEmpty();

    const key = try state.toKey(&self.key_scratch);
    defer key.deinit(self.allocator);

    return self.map.get(key) orelse blk: {
        const dst_blk = try self.dfa.getNew();

        // if the prong is not consuming the state of
        // the block will be accepting
        if (semi_empty) {
            var pass_instr = ir.Instr.initTag(.PRE_ACCEPT);
            const action = state.action.?;
            pass_instr.data = .{ .action = action };
            try dst_blk.insts.append(pass_instr);
        } else if (empty) {
            var pass_instr = ir.Instr.initTag(.RET);
            const action = state.action.?;
            pass_instr.data = .{ .action = action };
            try dst_blk.insts.append(pass_instr);
        }

        try self.put(try key.clone(self.allocator), dst_blk);

        break :blk dst_blk;
    };
}

fn put(self: *DfaGen, key: DfaState.Key, value: *ir.Block) !void {
    try self.map_keys.append(key);
    try self.map.putNoClobber(key, value);
}

pub fn deinit(self: *DfaGen) void {
    for (self.dfa_states.items) |dfa_state| dfa_state.deinit();
    for (self.map_keys.items) |key| key.deinit(self.allocator);

    self.key_scratch.deinit();
    self.dfa_states.deinit();
    self.map_keys.deinit();
    self.map.deinit();
}

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

    pub fn removeEmpty(self: *DfaState) void {
        var i: usize = 0;
        var index: usize = 0;
        const subs = self.sub_states.items;
        while (i < subs.len) : (i += 1) {
            const curr = &subs[i];
            if (curr.blocks.items.len == 0) {
                curr.deinit();
                continue;
            }

            subs[index] = curr.*;
            index += 1;
        }

        self.sub_states.shrinkRetainingCapacity(index);
    }

    pub fn isSemiEmpty(self: DfaState) bool {
        var found_empty = false;
        var found_full = false;

        for (self.sub_states.items) |sub| {
            const empty = sub.blocks.items.len == 0;
            found_full = found_full or !empty;
            found_empty = found_empty or empty;
        }

        return found_full and found_empty;
    }

    pub fn isEmpty(self: DfaState) bool {
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

        if (self.isSemiEmpty()) {
            assert(self.sub_states.items.len > 0);
            self.action = null;
            for (self.sub_states.items) |sub| {
                if (sub.blocks.items.len != 0) continue;
                self.action = sub.last_action;
                break;
            }
            assert(self.action != null);
        } else if (self.isEmpty()) {
            assert(self.sub_states.items.len > 0);
            self.action = self.sub_states.items[0].last_action;
        }
    }

    /// deduplicates the sub states of the dfa state
    fn deduplicate(self: *DfaState) void {
        if (self.sub_states.items.len <= 1) return;
        self.reorder();

        var i: usize = 1;
        var idx: usize = 1;
        while (i < self.sub_states.items.len) : (i += 1) {
            const last = self.sub_states.items[idx - 1];
            const curr = &self.sub_states.items[i];

            if (last.eql(curr.*)) {
                curr.deinit();
                continue;
            }

            self.sub_states.items[idx] = curr.*;
            idx += 1;
        }

        self.sub_states.shrinkRetainingCapacity(idx);
    }

    /// reordering the sub states of the dfa state to get
    /// a canonical representation of dfa states for hashing purposes
    fn reorder(self: *DfaState) void {
        const Ctx = struct {
            pub fn less(_: @This(), lhs: ra.ExecState, rhs: ra.ExecState) bool {
                const l_k = lhs.toUnownedKey();
                const r_k = rhs.toUnownedKey();

                if (l_k == null) return r_k != null;
                if (r_k == null) return false;

                return l_k.?.lessThan(r_k.?);
            }
        };

        std.sort.pdq(ra.ExecState, self.sub_states.items, Ctx{}, Ctx.less);
    }

    pub fn clone(self: DfaState) !DfaState {
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

        if (new_dfa_state.isSemiEmpty() or new_dfa_state.isEmpty()) {
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
            try state.addBranches(&prongs, true);
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
            const sub_states = try allocator.alloc(ra.ExecState.Key, self.sub_states.len);
            for (sub_states, self.sub_states) |*s, o| s.* = try o.clone(allocator);
            return .{ .sub_states = sub_states, .action = self.action };
        }

        pub fn deinit(self: Key, allocator: Allocator) void {
            for (self.sub_states) |k| k.deinit(allocator);
            allocator.free(self.sub_states);
        }

        pub fn eql(self: Key, other: Key) bool {
            if (self.sub_states.len != other.sub_states.len or
                other.action != self.action) return false;

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
                if (self.action) |act| {
                    try writer.print("(∅ , {d})", .{act});
                } else {
                    try writer.print("(∅ )", .{});
                }
                return;
            }

            try writer.print("{{ ", .{});
            for (self.sub_states) |sub_key| {
                try writer.print("{s} ", .{sub_key});
            }
            try writer.print("}}", .{});
        }
    };

    pub fn toKey(self: *DfaState, scratch: *std.AutoHashMap(usize, usize)) !Key {
        self.reorder();

        var sub_states = std.ArrayList(ra.ExecState.Key)
            .init(self.sub_states.allocator);

        for (self.sub_states.items) |*state| {
            if (try state.toKey(scratch)) |key| {
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
            if (self.action) |act| {
                try writer.print("(∅ , {d})", .{act});
            } else {
                try writer.print("(∅ )", .{});
            }
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
