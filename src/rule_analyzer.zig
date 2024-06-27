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

pub const AcceptanceSet = struct {
    values: [4]u64 = .{0} ** 4,

    const RangeIterator = struct {
        set: AcceptanceSet,
        index: u8,

        pub fn next(self: *RangeIterator) ?ir.Range {
            if (self.index == 255) return null;

            while (self.index < 255 and
                !self.set.matchesChar(self.index)) : (self.index += 1)
            {}

            const from = if (self.index == 255 and !self.set.matchesChar(self.index))
                return null
            else
                self.index;

            while (self.index < 255 and
                self.set.matchesChar(self.index)) : (self.index += 1)
            {}

            const to = if (self.index == 255 and self.set.matchesChar(self.index))
                255
            else
                self.index - 1;

            return .{
                .from = from,
                .to = to,
            };
        }
    };

    pub fn eql(self: AcceptanceSet, other: AcceptanceSet) bool {
        return std.mem.eql(u64, &self.values, &other.values);
    }

    pub fn rangeIter(self: AcceptanceSet) RangeIterator {
        return .{
            .set = self,
            .index = 0,
        };
    }

    /// merges two acceptance sets
    pub fn merge(self: *AcceptanceSet, other: AcceptanceSet) void {
        for (&self.values, &other.values) |*main_val, incoming_val| {
            main_val.* |= incoming_val;
        }
    }

    pub fn invert(self: *AcceptanceSet) void {
        for (&self.values) |*value| value.* = ~value.*;
    }

    pub fn cut(self: *AcceptanceSet, other: AcceptanceSet) void {
        for (&self.values, &other.values) |*main_val, incoming_val| {
            main_val.* &= ~incoming_val;
        }
    }

    pub fn disjoint(self: AcceptanceSet, other: AcceptanceSet) bool {
        var clone = self;
        clone.intersect(other);
        return clone.isEmpty();
    }

    pub fn intersect(self: *AcceptanceSet, other: AcceptanceSet) void {
        for (&self.values, &other.values) |*main_val, incoming_val| {
            main_val.* &= incoming_val;
        }
    }

    /// inclusive range
    pub fn addRange(self: *AcceptanceSet, from: u8, to: u8) void {
        for (from..to + 1) |char| self.addChar(@intCast(char));
    }

    pub fn addChar(self: *AcceptanceSet, char: u8) void {
        self.values[char >> 6] |= @as(u64, 1) << @as(u6, @intCast(char & 0x3F));
    }

    pub fn matchesChar(self: AcceptanceSet, char: u8) bool {
        return self.values[char >> 6] & (@as(u64, 1) << @as(u6, @intCast(char & 0x3F))) != 0;
    }

    /// inclusive range
    pub fn matchesRange(self: AcceptanceSet, from: u8, to: u8) bool {
        for (from..to + 1) |char| if (!self.matchesChar(char)) return false;
        return true;
    }

    pub fn isFull(self: AcceptanceSet) bool {
        for (0..4) |i| if (self.values[i] != std.math.maxInt(u64)) return false;
        return true;
    }

    pub fn isEmpty(self: AcceptanceSet) bool {
        for (0..4) |i| if (self.values[i] != 0) return false;
        return true;
    }

    pub fn subSet(self: AcceptanceSet, other: AcceptanceSet) bool {
        var cp = self;
        cp.cut(other);
        return cp.isEmpty();
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        var iter = self.rangeIter();
        while (iter.next()) |range| {
            try writer.print("{s}, ", .{range});
        }
    }
};

pub const Automaton = struct {
    blocks: std.ArrayList(*ir.Block),
    start: *ir.Block = undefined,
    fail: *ir.Block = undefined,

    pub fn init(allocator: Allocator) Automaton {
        return .{
            .blocks = std.ArrayList(*ir.Block).init(allocator),
        };
    }

    pub fn deinit(self: Automaton, allocator: Allocator) void {
        for (self.blocks.items) |block| block.deinit(allocator);
        self.blocks.deinit();
    }

    pub fn getNew(self: *Automaton) !*ir.Block {
        const block = try ir.Block.init(self.blocks.allocator);
        block.id = self.blocks.items.len;
        try self.blocks.append(block);
        return block;
    }

    pub fn isNfa(self: Automaton) bool {
        for (self.blocks.items.len) |block| {
            const instrs = block.insts.items;
            if (instrs.len != 1) return false;
            switch (instrs[0].tag) {
                .MATCH,
                .RET,
                .FAIL,
                .JMP,
                => {},
                else => return false,
            }
        }

        return true;
    }

    pub fn isDfa(self: Automaton) bool {
        for (self.blocks.items.len) |block| {
            const instrs = block.insts.items;
            if (instrs.len != 1) return false;
            switch (instrs[0].tag) {
                .MATCH,
                .RET,
                .FAIL,
                => {},
                else => return false,
            }

            if (block.fail != null and block.fail != self.fail) {
                return false;
            }
        }

        return true;
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

pub const SplitBranch = struct {
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

/// converts the acceptance sets of the branches int all distincs sets for all states
/// `prongs` are all the prongs to canonicalize
/// the function returns the set of all matches by the prongs
pub fn canonicalizeBranches(prongs: *std.ArrayList(SplitBranch)) !AcceptanceSet {
    var normal_match: AcceptanceSet = .{};

    try canonicalizeBase(prongs, &normal_match);
    try canonicalizeAccepts(prongs, &normal_match);

    return normal_match;
}

fn canonicalizeAccepts(
    prongs: *std.ArrayList(SplitBranch),
    normal_match: *AcceptanceSet,
) !void {
    // this strategy to choose acceptance states does not follow peg orderedness
    // and is essentially random.
    // maybe it would be nice to add a warning to the user or an error
    var i: usize = 1;
    while (i <= prongs.items.len) : (i += 1) {
        const prong = &prongs.items[i - 1];
        if (prong.final_action == null) continue;
        prong.set.cut(normal_match.*);
        if (prong.set.isEmpty()) {
            _ = prongs.swapRemove(i - 1);
            i -= 1;
        } else {
            normal_match.merge(prong.set);
        }
    }
}

fn canonicalizeBase(
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

/// the exec state is a basic execution state for ir blocks
/// (it does not handle lookahead, but anything else)
pub const ExecState = struct {
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

    pub fn setRoot(self: *ExecState) void {
        if (self.blocks.items.len == 0) return;
        const blk = self.blocks.getLast();
        self.blocks.clearRetainingCapacity();

        // blk was already in the stack
        // so there is place for it
        self.blocks.append(blk) catch unreachable;
    }

    pub fn splitOn(self: *const ExecState, set: AcceptanceSet) !?ExecState {
        const instr = self.getCurrInstr() orelse return null;

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
        const wrapped_instr = self.getCurrInstr();
        if (wrapped_instr == null) {
            // passing state
            try list.append(SplitBranch.initMatchAll(self.last_action));
            return;
        }

        const instr = wrapped_instr.?;
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

    pub fn getCurrInstr(self: ExecState) ?ir.Instr {
        if (self.blocks.items.len == 0) return null;

        const last = self.blocks.getLast();
        return last.insts.items[self.instr];
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

    pub const ExecJmpsResult = enum {
        LOOKAHEAD,
        CHANGE,
        NO_CHANGE,
    };

    // returns true if some jump could be executed
    pub fn execJumps(self: *ExecState) !ExecJmpsResult {
        const instr = self.getCurrInstr() orelse return .NO_CHANGE;
        const blocks = self.blocks.items;

        if (!instr.meta.isConsuming() or
            instr.tag == .MATCH and ir.isMatchLookahead(instr)) return .LOOKAHEAD;

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
            else => return .NO_CHANGE,
        }

        self.instr = 0;
        return .CHANGE;
    }

    pub fn clone(self: ExecState) !ExecState {
        return .{
            .instr_sub_idx = self.instr_sub_idx,
            .blocks = try self.blocks.clone(),
            .had_fill = self.had_fill,
            .instr = self.instr,
            .last_action = 0,
        };
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

        pub fn hash(self: Key, hasher: anytype) void {
            hasher.update(std.mem.asBytes(&self.instr));
            hasher.update(std.mem.asBytes(&self.instr_sub_idx));
            hasher.update(std.mem.asBytes(&@intFromPtr(self.block)));
        }

        pub fn lessThan(self: Key, other: Key) bool {
            return self.block.id < other.block.id or
                self.block.id == other.block.id and (self.instr < other.instr or
                self.instr == other.instr and self.instr_sub_idx < other.instr_sub_idx);
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

    pub fn toKey(self: ExecState) ?Key {
        if (self.blocks.items.len == 0) return null;

        return .{
            .instr_sub_idx = self.instr_sub_idx,
            .block = self.blocks.getLast(),
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

pub const ExecStart = struct {
    block_id: usize,
    instr: usize,

    pub fn init(id: usize, instr: usize) ExecStart {
        return .{ .block_id = id, .instr = instr };
    }
};
