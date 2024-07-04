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
const pir = @import("peg_ir.zig");
const su = @import("string_utils.zig");

pub const LowIr = struct {
    allocator: Allocator,
    file_name: []const u8,
    parser_name: []const u8,
    top_header: []const u8,
    field_header: []const u8,
    rule_names: [][]const u8,
    top_level_comment: []const []const u8,
    blocks: std.ArrayList(*Block),
    is_acceptor: bool,
    actions: std.ArrayList(Action),
    place_to_block: std.AutoHashMap(Label, *Block),

    pub fn deinit(self: *LowIr) void {
        const allocator = self.allocator;

        for (self.blocks.items) |blk| blk.deinit(allocator);
        self.blocks.deinit();

        for (self.actions.items) |*action| action.deinit(allocator);
        self.actions.deinit();

        self.place_to_block.deinit();
        allocator.free(self.rule_names);
        allocator.free(self.top_level_comment);
    }

    pub fn appendBlock(self: *LowIr, block: *Block, label: Label) !void {
        try self.appendDefBlock(block, label.def);
        try self.place_to_block.put(label, block);
    }

    pub fn appendDefBlock(self: *LowIr, block: *Block, def: usize) !void {
        block.id = self.blocks.items.len;
        block.base = def;
        try self.blocks.append(block);
    }

    /// appends a block with a single instruction
    pub fn appendInstr(self: *LowIr, instr: Instr, label: Label) !void {
        var blk = Block.init(self.allocator);
        blk.insts.append(instr);
        try self.appendBlock(blk, label);
    }

    /// makes the block bases continuous
    pub fn compressBases(self: *LowIr) !void {
        // this supposes that there is an upper bound the the
        // max base in the amount of blocks
        const blocks = self.blocks.items;
        for (blocks) |block| assert(block.base < blocks.len);

        var lookup = try self.allocator.alloc(usize, blocks.len);
        defer self.allocator.free(lookup);
        @memset(lookup, 0);

        var next: usize = 0;
        for (blocks) |block| {
            if (lookup[block.base] == 0) {
                lookup[block.base] = next + 1;
                next += 1;
            }

            block.base = lookup[block.base] - 1;
        }
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.blocks.items) |block| {
            try writer.print("{s}\n", .{block});
        }
    }
};

pub const Block = struct {
    /// the instructions in the block
    /// (should be ending with a jump or a return)
    insts: std.ArrayList(Instr),

    /// the fail label for the block
    fail: ?*Block,

    /// the identifier of the block
    id: usize,

    /// the identifier for the nonterminal to block belongs to
    base: usize,

    /// various other block infos
    meta: BlockMeta,

    pub fn init(allocator: Allocator) !*Block {
        const b = try allocator.create(Block);
        b.* = .{
            .insts = std.ArrayList(Instr).init(allocator),
            .fail = null,
            .id = 0,
            .base = 0,
            .meta = BlockMeta.Empty,
        };

        return b;
    }

    pub fn clone(self: *Block, allocator: Allocator) !*Block {
        const b = try allocator.create(Block);
        var insts = std.ArrayList(Instr).init(allocator);
        for (self.insts.items) |instr| {
            try insts.append(try instr.clone(allocator));
        }

        b.* = .{
            .fail = self.fail,
            .insts = insts,
            .id = 0,
            .base = 0,
            .meta = self.meta,
        };

        return b;
    }

    pub fn canFail(self: *Block) bool {
        for (self.insts.items) |instr| {
            switch (instr.tag) {
                .STRING,
                .MATCH,
                .NONTERM,
                => {
                    self.fail = null;
                    return true;
                },
                .FAIL,
                .RET,
                .EXIT_PASS,
                .EXIT_FAIL,
                => continue,
            }
        }

        return false;
    }

    pub fn deinit(self: *Block, allocator: Allocator) void {
        for (self.insts.items) |instr| {
            instr.deinit(allocator);
        }
        self.insts.deinit();
        self.fail = null;
        allocator.destroy(self);
    }

    pub fn format(
        self: Block,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.fail) |f| {
            try writer.print("%{d} -> !%{d}", .{ self.id, f.id });
        } else {
            try writer.print("%{d} -> !none", .{self.id});
        }

        try writer.print("{s} {{\n", .{self.meta});

        for (self.insts.items) |instr| {
            try writer.print("{s}\n", .{instr});
        }

        try writer.print("}}\n", .{});
    }
};

pub const BlockMeta = packed struct {
    mid_recurse: bool,
    right_recurse: bool,
    regular: bool,
    finite: bool,
    is_terminal: bool,
    nonterm_fail: bool,
    has_actions: bool,
    moves_actions: bool,
    is_target: bool,

    pub const Empty: BlockMeta = .{
        .mid_recurse = false,
        .right_recurse = false,
        .regular = false,
        .finite = false,
        .nonterm_fail = false,
        .has_actions = false,
        .moves_actions = false,
        .is_terminal = false,
        .is_target = false,
    };

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}{s}{s}{s}{s}{s}", .{
            if (self.mid_recurse) " (mr)" else if (self.right_recurse) " (rr)" else "",
            if (self.regular) " (reg)" else if (self.finite) " (fin)" else "",
            if (self.nonterm_fail) " (fail)" else "",
            if (self.moves_actions) " (act)" else "",
            if (self.is_terminal) " (ter)" else "",
            if (self.is_target) " (tgt)" else "",
        });
    }
};

pub const Label = struct {
    def: usize,
    choice: usize,
    symbol: usize,

    pub const Start: Label = .{
        .def = 0,
        .choice = 0,
        .symbol = 0,
    };

    pub fn larger(self: Label, other: Label) bool {
        if (self.def != other.def) return self.def > other.def;
        if (self.choice != other.choice) return self.choice > other.choice;
        return self.symbol > other.symbol;
    }

    pub fn next(self: Label) Label {
        var cp = self;
        cp.symbol += 1;
        return cp;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print(
            "$({d} {d} {d})",
            .{ self.def, self.choice, self.symbol },
        );
    }
};

pub const Instr = struct {
    tag: InstrTag,
    meta: InstrMeta,
    data: InstrData,

    pub fn initMetaTag(tag: InstrTag, meta: InstrMeta) Instr {
        var out = initTag(tag);
        out.meta = meta;
        return out;
    }

    pub fn initTag(tag: InstrTag) Instr {
        return .{
            .tag = tag,
            .meta = InstrMeta.Empty,
            .data = InstrData.Empty,
        };
    }

    pub fn initJmp(to: *Block) Instr {
        return .{
            .tag = .JMP,
            .meta = InstrMeta.Empty,
            .data = .{ .jmp = to },
        };
    }

    pub fn deinit(self: Instr, allocator: Allocator) void {
        switch (self.tag) {
            .MATCH => {
                for (self.data.match.items) |prong| prong.deinit();
                self.data.match.deinit();
            },
            .STRING => allocator.free(self.data.str),
            else => {},
        }
    }

    pub fn clone(self: Instr, allocator: Allocator) !Instr {
        return .{
            .tag = self.tag,
            .meta = self.meta,
            .data = switch (self.tag) {
                .MATCH => blk: {
                    const match = self.data.match;
                    var clone_list = std.ArrayList(MatchProng)
                        .init(allocator);

                    for (match.items) |prong| {
                        try clone_list.append(try prong.clone());
                    }
                    break :blk .{ .match = clone_list };
                },
                .STRING => blk: {
                    break :blk .{ .str = try allocator.dupe(u8, self.data.str) };
                },
                else => return self,
            },
        };
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self.tag) {
            .JMP => try writer.print(
                "  (jmp: %{d}{s})",
                .{ self.data.jmp.id, self.meta },
            ),
            .STRING => try writer.print(
                "  (string: \"{s}\"{s})",
                .{ self.data.str, self.meta },
            ),
            .MATCH => {
                try writer.print("  (match\n", .{});
                for (self.data.match.items) |prong| {
                    try writer.print("  {s}\n", .{prong});
                }
                try writer.print("  {s})", .{self.meta});
            },
            .NONTERM => try writer.print(
                "  (ctx jmp: %{d}, ret: %{d}{s})",
                .{
                    self.data.ctx_jmp.next.id,
                    self.data.ctx_jmp.returns.id,
                    self.meta,
                },
            ),
            .FAIL => try writer.print("  (fail)", .{}),
            .RET => try writer.print("  (ret: {d})", .{self.data.action}),
            .EXIT_PASS => try writer.print(
                "  (exit pass: {d})",
                .{self.data.action},
            ),
            .EXIT_FAIL => try writer.print("  (exit fail)", .{}),
        }
    }
};

pub const InstrTag = enum {
    STRING, // match a string
    JMP, // jump without context
    MATCH, // conditional jump on different ranges of chars
    NONTERM, // call a nonterminal
    FAIL, // return from the nonterminal and failed to match
    RET, // return from the nonterminal and succeeded to match
    EXIT_PASS, // exit from the loop and succeed to parse
    EXIT_FAIL, // exit and fail to parse
};

pub fn isMatchLookahead(instr: Instr) bool {
    assert(instr.tag == .MATCH);

    return instr.data.match.items.len == 1 and
        !instr.data.match.items[0].consuming;
}

pub const InstrData = union {
    str: []const u8,
    match: std.ArrayList(MatchProng),
    jmp: *Block,
    ctx_jmp: struct {
        next: *Block,
        returns: *Block,
    },
    action: usize,

    pub const Empty: InstrData = .{
        .str = "",
    };
};

pub const InstrMeta = struct {
    pos: bool,
    neg: bool,

    pub fn isConsuming(self: InstrMeta) bool {
        return !self.pos and !self.neg;
    }

    pub const Empty: InstrMeta = .{
        .pos = false,
        .neg = false,
    };

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}", .{
            if (self.pos)
                " (&)"
            else if (self.neg)
                " (!)"
            else
                "",
        });
    }
};

pub const MatchProng = struct {
    labels: std.ArrayList(Range),
    dest: *Block,
    consuming: bool,

    pub fn deinit(self: MatchProng) void {
        self.labels.deinit();
    }

    pub fn clone(self: MatchProng) !MatchProng {
        return .{
            .labels = try self.labels.clone(),
            .dest = self.dest,
            .consuming = self.consuming,
        };
    }

    pub fn mergeUnsafe(
        self: MatchProng,
        other: MatchProng,
        allocator: Allocator,
    ) !MatchProng {
        var new: MatchProng = .{
            .labels = std.ArrayList(Range).init(allocator),
            .dest = self.dest,
            .consuming = self.consuming,
        };

        try new.labels.appendSlice(self.labels.items);
        try new.labels.appendSlice(other.labels.items);
        try new.optimize();

        return new;
    }

    pub fn isOverlapping(self: MatchProng, other: MatchProng) bool {
        for (self.labels.items) |ours| for (other.labels.items) |theirs| {
            if (ours.overlaps(theirs)) return true;
        };

        return false;
    }

    pub fn invert(self: *MatchProng) !void {
        try self.optimize();
        const old_labels = self.labels;
        defer old_labels.deinit();
        self.labels = std.ArrayList(Range).init(self.labels.allocator);
        if (old_labels.items.len == 0) {
            try self.labels.append(.{ .from = 0, .to = 255 });
            return;
        }

        var left: u8 = 0;
        var right: u8 = old_labels.items[0].from;
        if (right != 0) {
            try self.labels.append(.{ .from = 0, .to = right - 1 });
        }

        left = old_labels.items[0].to;
        if (left == 255) return;
        left += 1;

        for (old_labels.items[1..]) |range| {
            right = range.from - 1;
            try self.labels.append(.{ .from = left, .to = right });
            left = if (range.to == 255) return else range.to + 1;
        }

        try self.labels.append(.{ .from = left, .to = 255 });
    }

    pub fn optimize(self: *MatchProng) !void {
        const labels = self.labels.items;
        std.sort.pdq(Range, labels, void{}, struct {
            pub fn inner(_: void, lhs: Range, rhs: Range) bool {
                return lhs.from < rhs.from;
            }
        }.inner);

        var place: usize = 0;
        var copy: usize = 0;

        while (copy < labels.len) : (copy += 1) {
            if (copy + 1 < labels.len and
                labels[copy].canMerge(labels[copy + 1]))
            {
                labels[copy + 1] = labels[copy].mergeUnsafe(labels[copy + 1]);
                continue;
            }

            labels[place] = labels[copy];
            place += 1;
        }

        self.labels.shrinkRetainingCapacity(place);
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.labels.items) |range| {
            try writer.print("{s}, ", .{range});
        }
        try writer.print(" => %{d}", .{self.dest.id});
    }
};

pub const Range = struct {
    from: u8,
    to: u8,

    pub fn initChar(char: u8) Range {
        return .{
            .from = char,
            .to = char,
        };
    }

    pub fn isChar(self: Range) bool {
        assert(self.to != 0 or self.from == 0);
        return self.to == self.from;
    }

    pub fn mergeUnsafe(self: Range, other: Range) Range {
        return .{
            .from = @min(self.from, other.from),
            .to = @max(self.to, other.to),
        };
    }

    pub fn canMerge(self: Range, other: Range) bool {
        if (self.isChar()) {
            return other.canMergeChar(self.from);
        }

        if (other.isChar()) {
            return self.canMergeChar(other.from);
        }

        return self.canMergeChar(other.from) or
            self.canMergeChar(other.to) or
            other.canMergeChar(self.from) or
            other.canMergeChar(self.to);
    }

    pub fn canMergeChar(self: Range, char: u8) bool {
        if (self.isChar()) {
            return self.from == char or
                self.from > char and self.from == char + 1 or
                self.from < char and self.from + 1 == char;
        }
        return self.contains(char) or
            self.from > char and char + 1 == self.from or
            self.to < char and char == self.to + 1;
    }

    pub fn overlaps(self: Range, other: Range) bool {
        return self.contains(other.from) or
            self.contains(other.to) or
            other.contains(self.from) or
            other.contains(self.to);
    }

    pub fn contains(self: Range, char: u8) bool {
        return self.isChar() and char == self.from or
            char >= self.from and char <= self.to;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try su.writeChar(writer, self.from);

        if (self.isChar()) return;

        try writer.print(" .. ", .{});
        try su.writeChar(writer, self.to);
    }
};

pub const Action = struct {
    args: std.ArrayList(LabeledRet),
    nonterms: std.ArrayList(CallPattern),
    bases: std.ArrayList(pir.ActionBase),

    // the last offset to increment the acc by
    last_offset: usize,
    base: *Block,

    pub fn init(
        allocator: Allocator,
        args: std.ArrayList(LabeledRet),
        base: *Block,
        nonterms: std.ArrayList(CallPattern),
        last_offset: usize,
        bases: std.ArrayList(pir.ActionBase),
    ) !Action {
        var args_cp = std.ArrayList(LabeledRet).init(allocator);
        for (args.items) |arg| {
            try args_cp.append(try arg.clone(allocator));
        }
        var bases_cp = std.ArrayList(pir.ActionBase).init(allocator);
        for (bases.items) |b| {
            try bases_cp.append(try b.clone(allocator));
        }

        return .{
            .nonterms = try nonterms.clone(),
            .last_offset = last_offset,
            .base = base,
            .args = args_cp,
            .bases = bases_cp,
        };
    }

    pub fn isEmpty(self: Action) bool {
        return self.bases.items.len == 0;
    }

    pub fn deinit(self: *Action, allocator: Allocator) void {
        for (self.args.items) |*arg| arg.deinit(allocator);
        self.args.deinit();
        for (self.bases.items) |*b| b.deinit(allocator);
        self.bases.deinit();
        self.nonterms.deinit();
    }
};

pub const CallPattern = struct {
    consumes: usize,
    calls: *Block,
};

pub const LabeledRet = struct {
    ret: pir.ReturnType,
    place: *Block,
    needs_mut: bool,
    lbl: usize,

    pub fn clone(self: LabeledRet, allocator: Allocator) !LabeledRet {
        return .{
            .lbl = self.lbl,
            .ret = try self.ret.clone(allocator),
            .place = self.place,
            .needs_mut = self.needs_mut,
        };
    }

    pub fn deinit(self: *LabeledRet, allocator: Allocator) void {
        self.ret.deinit(allocator);
    }
};
