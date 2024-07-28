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

pub fn MultiLevelBuffer(comptime T: type) type {
    return struct {
        base: std.ArrayList(T),
        last_sub: usize = 0,

        pub const Buffer = struct {
            ptr: *MultiLevelBuffer(T),
            start: usize,
            id: usize,

            pub fn append(self: *@This(), item: T) !void {
                assert(self.ptr.last_sub == self.id);
                try self.ptr.base.append(item);
            }

            pub fn deinit(self: *@This()) void {
                self.clear();
                self.ptr.last_sub -= 1;
            }

            pub fn clear(self: *@This()) void {
                self.shrink(0);
            }

            pub fn shrink(self: @This(), size: usize) void {
                assert(self.ptr.last_sub == self.id);
                assert(size <= self.items().len);

                self.ptr.base.shrinkRetainingCapacity(self.start + size);
            }

            pub fn items(self: @This()) []T {
                assert(self.start <= self.ptr.base.items.len);
                return self.ptr.base.items[self.start..];
            }

            /// append the element at `index` of this buffer to
            /// the last buffer
            pub fn appendBack(self: *@This(), index: usize) void {
                assert(index < self.items().len);
                const slice = self.ptr.base.items;
                std.mem.swap(T, &slice[self.start], &slice[self.start + index]);
                self.start += 1;
            }

            /// append the element at `index` of this buffer to the buffer
            /// before the last buffer
            pub fn appendBackTwo(self: *@This(), mid: *@This(), index: usize) void {
                assert(mid.id + 1 == self.id);
                assert(index < self.items().len);

                const slice = self.ptr.base.items;
                std.mem.swap(T, &slice[mid.start], &slice[self.start + index]);
                std.mem.swap(T, &slice[self.start], &slice[self.start + index]);
                self.start += 1;
                mid.start += 1;
            }
        };

        pub fn init(allocator: Allocator) @This() {
            return .{
                .base = std.ArrayList(T).init(allocator),
            };
        }

        pub fn getNew(self: *@This()) Buffer {
            self.last_sub += 1;
            return .{
                .ptr = self,
                .id = self.last_sub,
                .start = self.base.items.len,
            };
        }

        pub fn deinit(self: @This()) void {
            assert(self.last_sub == 0);
            self.base.deinit();
        }
    };
}

pub const AcceptanceSet = struct {
    values: [4]u64 = .{0} ** 4,

    pub const Full = blk: {
        var set: AcceptanceSet = .{};
        set.invert();
        break :blk set;
    };

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
        for (from..to) |char| self.addChar(@intCast(char));
        self.addChar(to);
    }

    pub fn addChar(self: *AcceptanceSet, char: u8) void {
        self.values[char >> 6] |= @as(u64, 1) << @as(u6, @intCast(char & 0x3F));
    }

    pub fn matchesChar(self: AcceptanceSet, char: u8) bool {
        return self.values[char >> 6] & (@as(u64, 1) << @as(u6, @intCast(char & 0x3F))) != 0;
    }

    /// inclusive range
    pub fn matchesRange(self: AcceptanceSet, from: u8, to: u8) bool {
        for (from..to) |char| if (!self.matchesChar(char)) return false;
        if (!self.matchesChar(to)) return false;
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

    /// this creates an automaton equivalen to the given instruction
    /// the lookahead information of the instruction gets ignored and
    /// should be added afterwards
    pub fn initInstr(allocator: Allocator, instr: ir.Instr) !Automaton {
        var self = init(allocator);

        const fail_blk = try self.getNew();
        try fail_blk.insts.append(ir.Instr.initTag(.TERM_FAIL));

        var curr_blk = try self.getNew();
        self.start = curr_blk;
        self.fail = fail_blk;

        switch (instr.tag) {
            .MATCH => {
                assert(instr.data.match.items.len == 1);
                try curr_blk.insts.append(try instr.clone(allocator));
                const branch = &curr_blk.instr.items[0].data.match.items[0];
                curr_blk = try self.getNew();
                branch.dest = curr_blk;
            },
            .STR => for (instr.data.str) |char| {
                const next = try self.getNew();
                try curr_blk.insts.append(try ir.Instr.initClass(
                    allocator,
                    .{ .from = char, .to = char },
                    next,
                    ir.InstrMeta.Empty,
                ));
                curr_blk = next;
            },
            else => unreachable,
        }

        var pass_instr = ir.Instr.initTag(.RET);
        pass_instr.data = .{ .action = 0 }; // the current default
        try curr_blk.insts.append(pass_instr);
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
        for (self.blocks.items) |block| {
            const instrs = block.insts.items;
            if (instrs.len != 1 and instrs.len != 2) return false;
            if ((instrs.len == 2) != (instrs[0].tag == .PRE_ACCEPT)) return false;
            if (instrs.len == 2) switch (instrs[1].tag) {
                .MATCH, .JMP => {},
                else => return false,
            } else switch (instrs[0].tag) {
                .MATCH, .JMP, .RET, .TERM_FAIL => {},
                else => return false,
            }
        }

        return true;
    }

    pub fn isDfa(self: Automaton) bool {
        for (self.blocks.items) |block| {
            const instrs = block.insts.items;
            if (instrs.len != 1 and instrs.len != 2) return false;
            if ((instrs.len == 2) != (instrs[0].tag == .PRE_ACCEPT)) return false;
            if (instrs.len == 2) switch (instrs[1].tag) {
                .MATCH => {},
                else => return false,
            } else switch (instrs[0].tag) {
                .MATCH, .RET, .TERM_FAIL => {},
                else => return false,
            }

            if (block.fail != null and block.fail != self.fail) {
                return false;
            }
        }

        return true;
    }

    pub fn appendFailChain(self: Automaton, to: *ir.Block, chain: ?*ir.Block) bool {
        if (chain == null) return false;

        var blk = to;
        while (blk.fail) |fail| {
            if (blk.fail == self.fail) break;
            blk = fail;
        }

        blk.fail = chain.?;
        return true;
    }

    pub fn replaceBlock(self: *Automaton, blk: *ir.Block, repl: *ir.Block) void {
        for (self.blocks.items) |block| {
            const instr = &block.insts.items[block.insts.items.len - 1];

            switch (instr.tag) {
                .JMP => if (instr.data.jmp == blk) {
                    instr.data.jmp = repl;
                },
                .MATCH => for (instr.data.match.items) |*prong| {
                    if (prong.dest == blk) {
                        prong.dest = repl;
                    }
                },
                else => {},
            }
        }
    }

    pub fn setFail(self: *Automaton) void {
        for (self.blocks.items) |blk| {
            if (blk == self.fail) continue;
            blk.fail = self.fail;
        }
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

pub const BranchResult = struct {
    fail_set: AcceptanceSet,
    prongs: std.ArrayList(SplitBranch),

    pub fn deinit(self: BranchResult) void {
        self.prongs.deinit();
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

    pub fn initProng(prong: ir.MatchProng, ret_look: bool) SplitBranch {
        var self = init(null);

        for (prong.labels.items) |range| {
            self.set.addRange(range.from, range.to);
        }

        if (!prong.consuming and ret_look) {
            const next_block = prong.dest;
            assert(next_block.insts.items.len == 1);
            const instr = next_block.insts.items[0];
            assert(instr.tag == .RET);
            self.final_action = instr.data.action;
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
    var buf = std.ArrayList(SplitBranch).init(prongs.allocator);
    defer buf.deinit();

    try canonicalizeBase(prongs, &buf, &normal_match);
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
    buf: *std.ArrayList(SplitBranch),
    normal_match: *AcceptanceSet,
) !void {
    buf.clearRetainingCapacity();

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
            try buf.append(prong_cp);
        }

        try prongs.appendSlice(buf.items);
        buf.clearRetainingCapacity();
    }
}

pub fn mergeDest(allocator: Allocator, dfa: *Automaton) !void {
    for (dfa.blocks.items) |blk| {
        const insts = blk.insts.items;
        assert(insts.len > 0);

        const instr = if (insts[0].tag == .PRE_ACCEPT) &insts[1] else &insts[0];
        if (instr.tag != .MATCH) continue;

        var new_prongs = std.ArrayList(ir.MatchProng).init(allocator);
        outer: for (instr.data.match.items) |prong| {
            for (new_prongs.items) |*new| {
                if (new.dest == prong.dest and new.consuming == prong.consuming) {
                    try new.labels.appendSlice(prong.labels.items);
                    try new.optimize();
                    prong.deinit();
                    continue :outer;
                }
            }

            try new_prongs.append(prong);
        }

        instr.data.match.deinit();
        instr.data.match = new_prongs;
    }
}

pub fn deleteUnreachable(allocator: Allocator, dfa: *Automaton) !void {
    var tags = try allocator.alloc(bool, dfa.blocks.items.len);
    defer allocator.free(tags);
    @memset(tags, false);

    const Frame = struct {
        blk: *ir.Block,
        index: usize = 0,
    };
    const fail_num = dfa.fail.id;
    tags[fail_num] = true;

    var stack = std.ArrayList(Frame).init(allocator);
    defer stack.deinit();
    try stack.append(.{ .blk = dfa.start });

    while (stack.popOrNull()) |popped_frame| {
        var frame = popped_frame;
        const instr = frame.blk.insts.getLast();

        tags[frame.blk.id] = true;

        switch (instr.tag) {
            .JMP => if (frame.index == 0) {
                const dest = instr.data.jmp;

                frame.index += 1;
                try stack.append(frame);

                if (tags[dest.id]) continue;

                try stack.append(.{ .blk = instr.data.jmp });
            },
            .MATCH => if (frame.index < instr.data.match.items.len) {
                const new: Frame = .{
                    .blk = instr.data.match.items[frame.index].dest,
                };

                frame.index += 1;
                try stack.append(frame);
                if (tags[new.blk.id]) continue;

                try stack.append(new);
            },
            else => {},
        }
    }

    var i: usize = 0;
    var index: usize = 0;
    while (i < dfa.blocks.items.len) : (i += 1) {
        const blk = dfa.blocks.items[i];
        if (!tags[blk.id]) {
            blk.deinit(allocator);
            continue;
        }

        dfa.blocks.items[index] = blk;
        blk.id = index;
        if (i == fail_num) dfa.fail = blk;
        index += 1;
    }

    dfa.blocks.shrinkRetainingCapacity(index);
}

pub const LookaheadType = enum {
    POSITIVE,
    NEGATIVE,

    pub fn less(self: LookaheadType, other: LookaheadType) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }
};

pub fn compressMatches(allocator: Allocator, dfa: *Automaton) !void {
    var starts = try allocator.alloc(bool, dfa.blocks.items.len);
    defer allocator.free(starts);
    @memset(starts, false);

    // populate starts
    starts[dfa.start.id] = true;
    for (dfa.blocks.items) |blk| {
        const insts = blk.insts.items;
        assert(insts.len == 1 or insts.len == 2);
        const instr = blk.insts.getLast();

        if (instr.tag != .MATCH) continue;
        const prongs = instr.data.match.items;
        if (prongs.len == 1) continue;

        for (prongs) |p| starts[p.dest.id] = true;
    }

    for (dfa.blocks.items) |blk| {
        if (!starts[blk.id]) continue;
        var match_string = std.ArrayList(u8).init(allocator);

        var curr_blk = blk;
        while (getMatchChar(&curr_blk)) |c| {
            try match_string.append(c);
        }

        const insts = curr_blk.insts.items;
        assert(insts.len > 0);
        if (insts.len == 2 and insts[0].tag != .PRE_ACCEPT) {
            const c_instr = &insts[0];
            assert(c_instr.tag == .STRING);

            try match_string.appendSlice(c_instr.data.str);
            assert(insts[1].tag == .JMP);
            curr_blk = insts[1].data.jmp;
        }

        if (match_string.items.len == 0) {
            match_string.deinit();
            continue;
        }

        const new_instr = ir.Instr.initString(try match_string.toOwnedSlice());
        const instr = &blk.insts.items[0];
        instr.deinit(allocator);
        instr.* = new_instr;

        try blk.insts.append(ir.Instr.initJmp(curr_blk));
    }

    try deleteUnreachable(allocator, dfa);
}

fn getMatchChar(blk: **ir.Block) ?u8 {
    if (blk.*.insts.items.len != 1) return null;
    const instr = &blk.*.insts.items[0];

    if (instr.tag != .MATCH) return null;
    const prongs = instr.data.match.items;
    if (prongs.len != 1) return null;
    const prong = prongs[0];
    if (prong.labels.items.len != 1 or !prong.consuming) return null;
    const class = prong.labels.items[0];
    if (!class.isChar()) return null;

    blk.* = prong.dest;
    return class.from;
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

    pub fn nextPlace(self: ExecState, set: AcceptanceSet) ?ExecPlace {
        const instr = self.getCurrInstr() orelse return null;

        assert(!set.isEmpty());

        switch (instr.tag) {
            .STRING => if (set.matchesChar(instr.data.str[self.instr_sub_idx])) {
                if (self.instr_sub_idx + 1 == instr.data.str.len) {
                    return .{
                        .block_id = self.blocks.getLast().id,
                        .instr = self.instr + 1,
                    };
                } else {
                    return .{
                        .block_id = self.blocks.getLast().id,
                        .instr = self.instr,
                        .instr_sub_idx = self.instr_sub_idx + 1,
                    };
                }
            },
            .MATCH => for (instr.data.match.items) |prong| {
                var prong_set: AcceptanceSet = .{};
                for (prong.labels.items) |range| prong_set.addRange(range.from, range.to);
                if (!set.subSet(prong_set)) continue;

                return .{ .block_id = prong.dest.id };
            },
            else => unreachable,
        }

        return null;
    }

    pub fn splitOn(self: ExecState, set: AcceptanceSet) !?ExecState {
        assert(!set.isEmpty());

        const instr = self.getCurrInstr() orelse return null;
        switch (instr.tag) {
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

    pub fn defaultPass(self: ExecState) ?usize {
        return if (self.blocks.items.len == 0) self.last_action else null;
    }

    pub fn skipPreAccept(self: *ExecState) bool {
        const instr = self.getCurrInstr() orelse return false;
        if (instr.tag == .PRE_ACCEPT) self.instr += 1;
        return instr.tag == .PRE_ACCEPT;
    }

    pub fn addBranches(
        self: ExecState,
        list: *std.ArrayList(SplitBranch),
        ret_look: bool,
    ) !void {
        const wrapped_instr = self.getCurrInstr();
        if (wrapped_instr == null) {
            // passing state
            try list.append(SplitBranch.initMatchAll(self.last_action));
            return;
        }

        const instr = wrapped_instr.?;
        switch (instr.tag) {
            .STRING => try list.append(SplitBranch
                .initChar(instr.data.str[self.instr_sub_idx])),
            .MATCH => for (instr.data.match.items) |prong| {
                try list.append(SplitBranch.initProng(prong, ret_look));
            },
            .RET => try list.append(SplitBranch.initMatchAll(self.last_action)),
            else => unreachable,
        }
    }

    /// assumes the current instr is always correct but not always a block
    pub fn getCurrInstr(self: ExecState) ?ir.Instr {
        const last = self.blocks.getLastOrNull() orelse return null;
        return last.insts.items[self.instr];
    }

    /// returns null on a bad instr but assumes there is a block
    pub fn getCurrInstrOrNull(self: ExecState) ?ir.Instr {
        const last = self.blocks.getLast();
        return if (last.insts.items.len <= self.instr)
            null
        else
            last.insts.items[self.instr];
    }

    pub fn canFillBranches(self: ExecState) bool {
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

    pub fn canExecJumps(self: ExecState) bool {
        const instr = self.getCurrInstr() orelse return false;
        return switch (instr.tag) {
            .JMP, .NONTERM, .RET => true,
            .FAIL, .EXIT_FAIL, .EXIT_PASS => unreachable,
            else => false,
        };
    }

    /// returns true if some jump could be executed
    pub fn execJumps(self: *ExecState) !ExecJmpsResult {
        const instr = self.getCurrInstr() orelse return .NO_CHANGE;
        if (!instr.meta.isConsuming() or
            instr.tag == .MATCH and ir.isMatchLookahead(instr)) return .LOOKAHEAD;

        return try self.execForceJumps();
    }

    /// same as exec jumps but ignoring lookaheads
    pub fn execForceJumps(self: *ExecState) !ExecJmpsResult {
        const instr = self.getCurrInstr() orelse return .NO_CHANGE;
        const blocks = self.blocks.items;

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
            },
            .RET => {
                // the jump has context
                _ = self.blocks.pop();
                self.last_action = instr.data.action;
            },
            // this would be a full fail
            // should be unreachable as fill branches ignores the fail branch
            .FAIL, .EXIT_FAIL, .EXIT_PASS => unreachable,
            else => return .NO_CHANGE,
        }

        self.had_fill = false;
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
        blocks: []*ir.Block,

        pub fn eql(self: Key, other: Key) bool {
            return self.instr == other.instr and
                self.instr_sub_idx == other.instr_sub_idx and
                std.mem.eql(*ir.Block, self.blocks, other.blocks);
        }

        pub fn hash(self: Key, hasher: anytype) void {
            hasher.update(std.mem.asBytes(&self.instr));
            hasher.update(std.mem.asBytes(&self.instr_sub_idx));
            for (self.blocks) |block| hasher.update(std.mem.asBytes(&block.id));
        }

        pub fn clone(self: Key, allocator: Allocator) !Key {
            return .{
                .instr = self.instr,
                .instr_sub_idx = self.instr_sub_idx,
                .blocks = try allocator.dupe(*ir.Block, self.blocks),
            };
        }

        pub fn lessThan(self: Key, other: Key) bool {
            if (self.instr < other.instr or
                self.instr == other.instr and self.instr_sub_idx < other.instr_sub_idx)
            {
                return true;
            }

            if (self.instr > other.instr or self.instr_sub_idx > other.instr_sub_idx) {
                return false;
            }

            if (self.blocks.len < other.blocks.len) return true;
            if (self.blocks.len > other.blocks.len) return false;
            for (self.blocks, other.blocks) |t, o| {
                if (t.id < o.id) return true;
                if (t.id > o.id) return false;
            }

            // they are equal
            return false;
        }

        pub fn deinit(self: Key, allocator: Allocator) void {
            allocator.free(self.blocks);
        }

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try writer.print("([", .{});
            for (self.blocks, 0..) |blk, i| {
                try writer.print("{d}", .{blk.id});
                if (i + 1 == self.blocks.len) break;
                try writer.print(", ", .{});
            }

            try writer.print("], {d}, {d})", .{
                self.instr,
                self.instr_sub_idx,
            });
        }
    };

    pub fn eql(self: ExecState, other: ExecState) bool {
        const self_top = self.getTopSlice();
        const other_top = other.getTopSlice();
        return std.mem.eql(*ir.Block, self_top, other_top) and
            self.instr == other.instr and
            self.instr_sub_idx == other.instr_sub_idx;
    }

    fn getTopSlice(self: ExecState) []*ir.Block {
        const blks = self.blocks.items;
        var iter = std.mem.reverseIterator(blks);
        while (iter.next()) |blk| {
            const last = blk.insts.getLast();
            if (!last.meta.isConsuming()) break;
        }
        return blks[iter.index..];
    }

    pub const SplitOffResult = struct {
        blk: *ir.Block,
        instr: usize,
        look: LookaheadType,

        pub fn toExecPlace(self: SplitOffResult) ExecPlace {
            return ExecPlace.init(self.blk.id, self.instr);
        }

        pub fn lessThan(_: void, self: SplitOffResult, other: SplitOffResult) bool {
            return self.blk.id < other.blk.id or
                self.blk.id == other.blk.id and (self.instr < other.instr or
                self.instr == other.instr and self.look.less(other.look));
        }

        pub fn eql(self: SplitOffResult, other: SplitOffResult) bool {
            return self.blk.id == other.blk.id and
                self.instr == other.instr and self.look == other.look;
        }
    };

    pub fn splitOff(self: *ExecState) SplitOffResult {
        const instr = self.getCurrInstr() orelse unreachable;
        const blocks = self.blocks.items;
        const curr_blk = &blocks[blocks.len - 1];

        assert(!instr.meta.isConsuming());
        var look: LookaheadType = undefined;
        const instr_idx = self.instr;
        switch (instr.tag) {
            .NONTERM => {
                look = if (instr.meta.pos) .POSITIVE else .NEGATIVE;

                // got to the next block
                curr_blk.* = instr.data.ctx_jmp.returns;
                self.instr = 0;
            },
            .STRING => {
                look = if (instr.meta.pos) .POSITIVE else .NEGATIVE;

                // string match cannot be the last instruction of a block
                self.instr += 1;
            },
            .MATCH => {
                look = .POSITIVE;

                // got to the next block

                // there should be only one way
                // (this causes no problems because lowering only generates one)
                assert(instr.data.match.items.len == 1);
                curr_blk.* = instr.data.match.items[0].dest;
                self.instr = 0;
            },
            else => unreachable,
        }

        self.instr_sub_idx = 0;
        return .{
            .blk = curr_blk.*,
            .instr = instr_idx,
            .look = look,
        };
    }

    pub fn toKey(self: ExecState, scratch: *std.AutoHashMap(usize, usize)) !?Key {
        if (self.blocks.items.len == 0) return null;

        const allocator = scratch.allocator;

        // start with getting the lowest sub nfa
        const last_slice = self.getTopSlice();
        scratch.clearRetainingCapacity();
        var key_stack = std.ArrayList(*ir.Block).init(allocator);

        for (last_slice) |blk| {
            if (scratch.get(blk.id)) |len| {
                key_stack.shrinkRetainingCapacity(len);
                continue;
            }

            try key_stack.append(blk);
            try scratch.putNoClobber(blk.id, key_stack.items.len);
        }

        return .{
            .instr_sub_idx = self.instr_sub_idx,
            .blocks = try key_stack.toOwnedSlice(),
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
            try writer.print("([", .{});
            const top_slice = self.getTopSlice();
            for (top_slice, 0..) |blk, i| {
                try writer.print("{d}", .{blk.id});
                if (i + 1 == top_slice.len) break;
                try writer.print(", ", .{});
            }

            try writer.print("], {d}, {d})", .{
                self.instr,
                self.instr_sub_idx,
            });
        }
    }

    pub fn deinit(self: ExecState) void {
        self.blocks.deinit();
    }
};

pub const ExecPlace = struct {
    block_id: usize = 0,
    instr: usize = 0,
    instr_sub_idx: usize = 0,

    pub fn init(id: usize, instr: usize) ExecPlace {
        return .{ .block_id = id, .instr = instr };
    }

    pub fn eql(self: ExecPlace, other: ExecPlace) bool {
        return self.block_id == other.block_id and
            self.instr == other.instr and
            self.instr_sub_idx == other.instr_sub_idx;
    }

    pub fn less(self: ExecPlace, other: ExecPlace) bool {
        if (self.block_id < other.block_id) return true;
        if (self.block_id > other.block_id) return false;
        if (self.instr < other.instr) return true;
        if (self.instr > other.instr) return false;
        return self.instr_sub_idx < other.instr_sub_idx;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("({d} {d} {d})", .{ self.block_id, self.instr, self.instr_sub_idx });
    }
};
