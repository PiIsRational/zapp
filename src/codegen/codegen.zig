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
const Writer = std.fs.File.Writer;

const lir = @import("../lir.zig").ir;
const su = @import("../string_utils.zig");
const pir = @import("../peg_ir.zig");
const Version = @import("config").Version;

const CodeGen = @This();

const imports =
    \\const std = @import("std");
    \\const Allocator = std.mem.Allocator;
    \\const assert = std.debug.assert;
;

const helper =
    \\fn invert(comptime T: type, s: []T) []T {
    \\    for (0..@divFloor(s.len, 2)) |i| {
    \\        const j = s.len - 1 - i;
    \\        std.mem.swap(T, &s[i], &s[j]);
    \\    }
    \\
    \\    return s;
    \\}
    \\
;

const structs =
    \\
    \\const State = u16;
    \\const Rule = u16;
    \\const CharIdx = u32;
    \\const CharSpanLength = u32;
    \\
    \\/// the maximal accepted input size for the parser
    \\/// (the -2 is because 0 and 1 represent empty cells and no math cells)
    \\pub const MaxInputSize = std.math.maxInt(u32) - 2; 
    \\
    \\pub const ParserError = error {
    \\    InputTooLarge,
    \\} || Allocator.Error;
    \\
    \\const UseMemo = true;
    \\
    \\const EvalFrame = struct {
    \\    stack_start: usize,
    \\    start: CharIdx,
    \\    acc: CharSpanLength,
    \\    state: State,
    \\    look: LookType,
    \\    empty_backtrack: bool,
    \\};
    \\
    \\pub const ParsingError = struct {
    \\    last_found: []const u8,
    \\    expected: []const u8,
    \\};
    \\
    \\pub const InferError = struct {
    \\    err: ?anyerror,
    \\    msg: []const u8,
    \\};
    \\
    \\
    \\const InferInstr = struct {
    \\    state: State,
    \\    length: CharSpanLength,
    \\    start: CharIdx,
    \\    memo: bool,
    \\};
    \\
    \\const InferFrame = struct {
    \\    acc: CharIdx,
    \\    state: State,
    \\};
    \\
    \\const LookType = struct {
    \\    inverted: bool,
    \\    consuming: bool,
    \\};
    \\
    \\pub const Stats = struct {
    \\    /// memory used by the memoization table (in bytes)
    \\    memo: usize,
    \\
    \\    /// the memory useage of the parse stack (in bytes)
    \\    parse: usize,
    \\
    \\    /// the memory useage of the infer stack (in bytes)
    \\    infer: usize,
    \\
    \\    /// the memory useage of the instruction stack (in bytes)
    \\    instr: usize,
    \\
    \\    /// the memory useage of the action return stack (in bytes)
    \\    calc: usize,
    \\
    \\    /// the maximal memory the memoization table needed (in columns)
    \\    max_memo: usize,
    \\
    \\    /// the rule in which the maximal table size was first found
    \\    max_memo_state: usize,
    \\
    \\    pub fn format(
    \\        self: Stats, 
    \\        comptime _: []const u8,
    \\        _: std.fmt.FormatOptions,
    \\        writer: anytype,
    \\    ) !void {
    \\        try writer.print(
    \\            "memo: {d}\nmax_memo: {d}\nparse: {d}\ninfer: {d}\ninstr: {d}\ncalc: {d}\n", 
    \\            .{self.memo, self.max_memo, self.parse, self.infer, self.instr, self.calc},
    \\        );
    \\    }
    \\};
    \\
    \\pub const ParseOptions = struct {
    \\    /// make use of memoization (defaults to true)
    \\    use_memo: bool = true,
    \\
    \\    /// set the limit to the parsing stack
    \\    /// per default it is unlimited
    \\    stack_limit: ?usize = null,
    \\      
    \\    /// the amount of chunks to use in the memoization table
    \\    /// per default it depends on the rule count
    \\    chunks: ?usize = null,
    \\};
    \\
;

const vars =
    \\chars: [:0]const u8,
    \\allocator: Allocator,
    \\stack: std.ArrayList(EvalFrame),
    \\stack_alloc: ?std.heap.FixedBufferAllocator,
    \\infer_stack: std.ArrayList(InferFrame),
    \\calc_stack: std.ArrayList(InferReturnType),
    \\infer_actions: std.ArrayList(InferInstr),
    \\infer_instrs: std.ArrayList(InferInstr),
    \\memo: MemoTable(.{ .rule_count = RuleCount, .chunks = opts.chunks }),
    \\state: State,
    \\start: CharIdx,
    \\acc: CharSpanLength,
    \\infer_acc: CharSpanLength,
    \\infer_done: bool,
    \\did_fail: bool,
    \\infer_state: State,
    \\infer_start: CharIdx,
    \\stack_start: usize,
    \\empty_backtrack: bool,
    \\max_reached: CharIdx,
    \\max_slice: []const u8,
    \\next_expected: State,
    \\max_memo_state: State,
    \\infer_fail_msg: []const u8,
    \\infer_fail_err: ?anyerror,
    \\term_end_state: State,
    \\has_term_accept: bool,
    \\term_accept_len: CharSpanLength,
;

const funcs =
    \\
    \\pub fn init(allocator: Allocator) Allocator.Error!@This() {
    \\    var self: @This() = undefined;
    \\    self.allocator = allocator;
    \\    if (opts.stack_limit) |limit| {
    \\        var stack_alloc = std.heap.FixedBufferAllocator
    \\            .init(try self.allocator.alloc(u8, @sizeOf(EvalFrame) * limit));
    \\        self.stack = std.ArrayList(EvalFrame)
    \\            .init(stack_alloc.allocator());
    \\        try self.stack.ensureTotalCapacity(limit);
    \\        self.stack_alloc = stack_alloc;
    \\    } else {
    \\        self.stack_alloc = null;
    \\        self.stack = std.ArrayList(EvalFrame).init(allocator);
    \\    }
    \\    self.infer_stack = std.ArrayList(InferFrame).init(allocator);
    \\    self.memo = MemoTable(.{ .rule_count = RuleCount, .chunks = opts.chunks })
    \\        .init(allocator);
    \\    self.calc_stack = std.ArrayList(InferReturnType).init(allocator);
    \\    self.infer_actions = std.ArrayList(InferInstr).init(allocator);
    \\    self.infer_instrs = std.ArrayList(InferInstr).init(allocator);
    \\    self.reset("");
    \\    return self;
    \\}
    \\
    \\pub fn deinit(self: *@This()) void {
    \\    self.infer_actions.deinit();
    \\    self.infer_instrs.deinit();
    \\    self.stack.deinit();
    \\    if (self.stack_alloc) |allocator| {
    \\        self.allocator.free(allocator.buffer);
    \\    }
    \\    self.infer_stack.deinit();
    \\    self.memo.deinit();
    \\    self.calc_stack.deinit();
    \\}
    \\
    \\fn reset(self: *@This(), chars: [:0]const u8) void {
    \\    assert(self.stack.items.len == 0);
    \\    assert(self.infer_stack.items.len == 0);
    \\    assert(self.calc_stack.items.len == 0);
    \\    self.infer_actions.clearRetainingCapacity();
    \\    self.infer_instrs.clearRetainingCapacity();
    \\    self.memo.reset();
    \\    self.state = 0;
    \\    self.infer_state = 0;
    \\    self.infer_start = 0;
    \\    self.stack_start = 0;
    \\    self.start = 0;
    \\    self.acc = 0;
    \\    self.infer_acc = 0;
    \\    self.infer_done = false;
    \\    self.did_fail = false;
    \\    self.max_reached = 0;
    \\    self.max_slice = "";
    \\    self.next_expected = 0;
    \\    self.infer_fail_msg = "";
    \\    self.infer_fail_err = null;
    \\    self.empty_backtrack = AddrToNFail[0];
    \\    self.max_memo_state = 0;
    \\    self.chars = chars;
    \\    self.term_end_state = 0;
    \\    self.has_term_accept = false;
    \\    self.term_accept_len = 0;
    \\}
    \\
    \\pub fn stats(self: @This()) Stats {
    \\    return .{
    \\        .memo = self.memo.getMemUseage(),
    \\        .max_memo = self.memo.max_chars,
    \\        .parse = self.stack.capacity * @sizeOf(EvalFrame),
    \\        .infer = self.infer_stack.capacity * @sizeOf(InferFrame),
    \\        .instr = self.infer_instrs.capacity * @sizeOf(InferInstr),
    \\        .calc = self.calc_stack.capacity * @sizeOf(InferReturnType),
    \\        .max_memo_state = AddrToRule[self.max_memo_state],
    \\    };
    \\}
    \\
    \\fn parseNonterminal(
    \\    self: *@This(), 
    \\    state: State, 
    \\    inverted: bool, 
    \\    consuming: bool,
    \\) Allocator.Error!void {
    \\    if (opts.use_memo) try self.adjustBacktrack();
    \\    try self.stack.append(.{
    \\        .start = self.start,
    \\        .acc = self.acc,
    \\        .state = self.state,
    \\        .stack_start = self.stack_start + self.memo.start,
    \\        .look = .{
    \\            .inverted = inverted,
    \\            .consuming = consuming,
    \\        },
    \\        .empty_backtrack = self.empty_backtrack,
    \\    });
    \\
    \\    self.stack_start = self.infer_actions.items.len;
    \\    self.start += self.acc;
    \\    self.state = state;
    \\    self.acc = 0;
    \\    self.empty_backtrack = AddrToNFail[self.state] and 
    \\        self.empty_backtrack and consuming;
    \\      
    \\    if (!opts.use_memo) return;
    \\    switch (self.memo.get(self.start, AddrToRule[state])) {
    \\        .pass => |val| { 
    \\            self.state = val.end_state;
    \\            self.acc = val.length;
    \\            try self.returnFromNonterminal(false, true);
    \\        },
    \\        .fail => |val| {
    \\            self.state = val.end_state;
    \\            try self.returnFromNonterminal(true, true);
    \\        },
    \\        .empty => return,
    \\    }
    \\}
    \\
    \\fn returnFromNonterminal(
    \\    self: *@This(), 
    \\    failed: bool, 
    \\    memo: bool, 
    \\) Allocator.Error!void {
    \\    const frame = self.stack.popOrNull() orelse blk: {
    \\        self.did_fail = failed;
    \\        break :blk EvalFrame{
    \\            .stack_start = 0,
    \\            .start = self.start,
    \\            .acc = 0,
    \\            .state = self.state,
    \\            .look = .{
    \\                .inverted = false,
    \\                .consuming = true,
    \\            },
    \\            .empty_backtrack = AddrToNFail[self.state],
    \\        };
    \\    };
    \\
    \\    const look = frame.look;
    \\    const good = failed == look.inverted;
    \\  
    \\    if (!memo and !failed and look.consuming and 
    \\        self.max_reached <= self.start + self.acc and 
    \\       (self.max_reached < self.start + self.acc or self.acc > self.max_slice.len)) {
    \\        self.max_slice = self.chars[self.start .. self.start + self.acc];
    \\        self.max_reached = self.start + self.acc;
    \\    } else if (self.max_reached == self.start) {
    \\        self.next_expected = self.state;
    \\    }
    \\
    \\    if (!memo and opts.use_memo) {
    \\        try self.memo.add(self.start, AddrToRule[self.state], 
    \\            if (failed)
    \\                .{ .fail = .{ .end_state = @intCast(self.state) } }
    \\            else 
    \\                .{ .pass = .{ 
    \\                    .length = @intCast(self.acc), 
    \\                    .end_state = @intCast(self.state), 
    \\                } }
    \\        );
    \\    }
    \\
    \\    if (look.consuming and good) {
    \\        try self.infer_actions.append(.{
    \\            .length = self.acc,
    \\            .state = self.state,
    \\            .start = self.start,
    \\            .memo = memo,
    \\        });
    \\        self.acc += frame.acc;
    \\        self.state = frame.state + 1;
    \\    } else if (good) {
    \\        self.infer_actions.shrinkRetainingCapacity(self.stack_start);
    \\        self.acc = frame.acc;
    \\        self.state = frame.state + 1;
    \\    } else {
    \\        if (frame.stack_start >= self.memo.start) {
    \\            self.infer_actions.shrinkRetainingCapacity(
    \\                frame.stack_start - self.memo.start,
    \\            );
    \\        }
    \\        self.acc = 0;
    \\        self.state = AddrToFailState[frame.state];
    \\    }
    \\
    \\    self.stack_start = frame.stack_start - @min(frame.stack_start, self.memo.start);
    \\    self.start = frame.start;
    \\    if (opts.use_memo) {
    \\        if (frame.empty_backtrack) {
    \\            self.empty_backtrack = true;
    \\            if (self.empty_backtrack) try self.resetMemo(self.acc + self.start);
    \\        } else try self.adjustBacktrack();
    \\    }
    \\}
    \\
    \\fn adjustBacktrack(self: *@This()) Allocator.Error!void {
    \\    if (self.stack.getLastOrNull()) |parent| {
    \\        self.empty_backtrack = parent.empty_backtrack 
    \\            and AddrToNFail[self.state] and parent.look.consuming;
    \\        if (self.empty_backtrack) try self.resetMemo(self.acc + self.start);
    \\    } else {
    \\        self.empty_backtrack = AddrToNFail[self.state];
    \\        if (self.empty_backtrack) try self.resetMemo(self.acc + self.start);
    \\    }
    \\}
    \\
    \\fn getParseError(self: @This()) ParsingError {
    \\    const rule = AddrToRule[self.next_expected];
    \\    return .{
    \\        .last_found = self.max_slice,
    \\        .expected = if (rule < NameTable.len) 
    \\            NameTable[rule] 
    \\        else "",
    \\    };
    \\}
    \\
;

const inferFuncs =
    \\fn finalizeInferInstrs(self: *@This()) Allocator.Error!void {
    \\    for (self.infer_actions.items) |frame| {
    \\        if (!opts.use_memo or !frame.memo) {
    \\            try self.infer_instrs.append(frame);
    \\        } else {
    \\            try self.memoInfer(frame.state, frame.start);
    \\        }
    \\    }
    \\    self.infer_actions.clearRetainingCapacity();
    \\    self.stack_start = 0;
    \\}
    \\
    \\fn fail(self: *@This(), msg: []const u8, err: ?anyerror) void {
    \\    self.did_fail = true;
    \\    self.infer_fail_err = err;
    \\    self.infer_fail_msg = msg;
    \\}
    \\
    \\fn inferNonterminal(self: *@This(), state: State) Allocator.Error!void {
    \\    try self.infer_stack.append(.{
    \\        .acc = self.infer_acc,
    \\        .state = self.infer_state,
    \\    });
    \\
    \\    const last_state = self.memo.getState(self.infer_acc, AddrToRule[state]);
    \\    self.infer_state = ActionTranslate[last_state];
    \\}
    \\
    \\fn skipInferNonterminal(self: *@This(), state: State) void {
    \\    const length = switch (self.memo.get(self.infer_acc, AddrToRule[state])) {
    \\        .pass => |val| val.length,
    \\        else => unreachable,
    \\    };
    \\
    \\    self.infer_acc += length;
    \\    self.infer_state += 1;
    \\}
    \\
    \\fn returnFromInfer(self: *@This(), state: State) Allocator.Error!void {
    \\    const frame = self.infer_stack.popOrNull() orelse blk: {
    \\        self.infer_done = true;
    \\        break :blk InferFrame{
    \\            .state = self.infer_state,
    \\            .acc = self.infer_start,
    \\        };
    \\    };
    \\
    \\    try self.infer_instrs.append(.{
    \\        .memo = false,
    \\        .start = frame.acc,
    \\        .state = state,
    \\        .length = self.infer_acc - frame.acc,
    \\    });
    \\    self.infer_state = frame.state + 1;
    \\}
;

writer: Writer,
ir: lir.LowIr,
parse_return: pir.ReturnType,

pub fn generate(
    self: *CodeGen,
    ir: lir.LowIr,
    writer: Writer,
) !void {
    self.writer = writer;
    self.ir = ir;
    try self.ir.compressBases();

    try writer.print(
        "// {s} parser generated by zapp {}. DO NOT EDIT!\n\n",
        .{ ir.parser_name, Version },
    );

    try self.genTopLevelComment();

    // imports
    try writer.print("{s}\n", .{imports});
    try writer.print("\n{s}\n", .{self.ir.top_header});

    // helper(s)
    try writer.print("{s}\n", .{helper});

    // type declarations
    try writer.writeAll(@embedFile("./memo.zig"));
    try self.returnTypes();
    try self.parseReturn();
    try writer.print("{s}\n", .{structs});

    // lookups
    try self.nameTable();
    try self.addrToRule();
    try self.addrToFailState();
    try self.actionTranslationTable();
    try self.addrToNFail();

    // fn start
    try writer.print(
        "pub fn @\"{s}\"(comptime opts: ParseOptions) type {{\n",
        .{ir.parser_name},
    );
    try writer.print("    return struct {{\n", .{});

    // fields
    try writer.print("{s}\n", .{vars});
    try writer.print("\n{s}\n", .{self.ir.field_header});

    // static funcs
    try writer.print("{s}\n", .{funcs});

    // generated funcs
    try self.resetMemo();
    try self.parse();

    if (self.ir.is_acceptor) {
        try writer.print("}};\n}}", .{});
        return;
    }

    // static infer funcs
    try writer.print("{s}\n", .{inferFuncs});

    // generated infer funcs
    try self.callAction();
    try self.memoInfer();
    try self.infer();

    // fn end
    try writer.print("}};\n}}", .{});
}

/// adds the return type for the parse function
fn parseReturn(self: *CodeGen) !void {
    const writer = self.writer;
    try writer.print("pub const ParseReturn = union(enum) {{\n", .{});
    try writer.print("    pass: {s},\n", .{self.parse_return});
    try writer.print("    parse_fail: ParsingError,\n", .{});
    try writer.print("    infer_fail: InferError,\n", .{});
    try writer.print("}};\n", .{});
}

fn genTopLevelComment(self: *CodeGen) !void {
    const writer = self.writer;

    for (self.ir.top_level_comment) |content| {
        try writer.print("//{s}\n", .{content});
    }

    if (self.ir.top_level_comment.len > 0) {
        try writer.print("\n", .{});
    }
}

fn resetMemo(self: *CodeGen) !void {
    const resetMemoRest =
        \\    if (new_begin < self.memo.start) {
        \\        self.did_fail = true;
        \\        return;
        \\    }
        \\    if (self.memo.base_table.items.len > self.memo.max_chars) 
        \\        self.max_memo_state = self.state;
        \\    self.memo.resetTo(new_begin);
        \\}
    ;
    try self.writer.print(
        "fn resetMemo(self: *@This(), new_begin: usize) Allocator.Error!void {{\n",
        .{},
    );

    if (!self.ir.is_acceptor) {
        try self.writer.print("try self.finalizeInferInstrs();\n", .{});
    }
    try self.writer.print("{s}\n", .{resetMemoRest});
}

/// lookup generation
/// for error messages
fn nameTable(self: *CodeGen) !void {
    const writer = self.writer;
    try writer.print("const NameTable = [_][]const u8{{\n", .{});

    for (self.ir.rule_names) |rule_name| {
        try su.writeString(writer, rule_name);
        try writer.print(",\n", .{});
    }

    try writer.print("}};\n", .{});
}

/// lookup generation for
/// finding the fail state an address would go to
fn addrToFailState(self: *CodeGen) !void {
    const blocks = self.ir.blocks.items;
    const writer = self.writer;

    try writer.print("const AddrToFailState = [_]State{{\n", .{});

    try writer.print("{d}", .{
        if (blocks[0].fail) |f| f.id else blocks.len,
    });

    for (blocks[1..]) |block| {
        try writer.print(", {d}", .{
            if (block.fail) |f| f.id else blocks.len,
        });
    }

    try writer.print("}};\n", .{});
}

/// lookup generation
fn addrToRule(self: *CodeGen) !void {
    const blocks = self.ir.blocks.items;
    try self.writer.print("const AddrToRule = [_]Rule{{\n", .{});

    const first = blocks[0].base;
    try self.writer.print("{d}", .{first});

    var max_rule: usize = 0;
    for (blocks[1..]) |block| {
        try self.writer.print(", {d}", .{block.base});
        max_rule = @max(max_rule, block.base);
    }

    try self.writer.print("}};\n", .{});
    try self.writer.print("const RuleCount = {d};\n", .{max_rule + 1});
}

fn addrToNFail(self: *CodeGen) !void {
    const blocks = self.ir.blocks.items;
    try self.writer.print("const AddrToNFail = [_]bool {{", .{});

    try self.writer.print("{}", .{blocks[0].meta.nonterm_fail});
    for (blocks[1..]) |block| {
        try self.writer.print(", {}", .{block.meta.nonterm_fail});
    }

    try self.writer.print("}};\n", .{});
}

fn actionTranslationTable(self: *CodeGen) !void {
    const blocks = self.ir.blocks.items;
    const allocator = self.ir.allocator;
    var table = try allocator.alloc(usize, blocks.len);
    defer allocator.free(table);
    @memset(table, 0);

    var count: usize = 0;
    for (self.ir.actions.items) |action| {
        const add = action.base.id;
        table[add] = count + 1;
        count += action.nonterms.items.len + 1;
    }

    var last: usize = count;
    for (0..blocks.len) |i| {
        const idx = blocks.len - 1 - i;
        if (table[idx] == 0) {
            table[idx] = blocks.len + 1;
        } else {
            // TODO: put this to work again afterwards (remove the comment)
            //assert(last >= table[idx]);
            last = table[idx];
        }
    }

    try self.writer.print("const ActionTranslate = [_]State{{\n", .{});

    try self.writer.print("{d}", .{table[0] - 1});
    for (1..blocks.len) |i| {
        try self.writer.print(", {d}", .{table[i] - 1});
    }

    try self.writer.print("}};\n", .{});
}

fn returnTypes(self: *CodeGen) !void {
    const allocator = self.ir.allocator;
    const actions = self.ir.actions.items;
    const blocks = self.ir.blocks.items;
    var lookup = try allocator.alloc(bool, blocks.len);
    @memset(lookup, false);
    defer allocator.free(lookup);

    try self.writer.print("const InferReturnType = union {{\n", .{});

    for (actions) |action| {
        const def = action.base.base;
        if (lookup[def]) continue;
        lookup[def] = true;

        const bases = action.bases.items;
        if (bases.len >= 1) {
            const last_base = bases[0];
            if (!last_base.return_type.isNone()) {
                try self.writer.print("type_{d}: {s},\n", .{ def, last_base.return_type });
            }
            if (def == 0) self.parse_return = last_base.return_type;
        } else if (def == 0) {
            self.parse_return = pir.ReturnType.empty();
        }
    }
    try self.writer.print("}};\n", .{});
}

fn parse(self: *CodeGen) !void {
    const parseStart =
        \\pub fn parse(
        \\    self: *@This(), 
        \\    chars: [:0]const u8, 
        \\) ParserError!ParseReturn {
        \\
        \\if (chars.len > MaxInputSize) {
        \\    return error.InputTooLarge;
        \\}
        \\
        \\self.reset(chars);
        \\
        \\while (true) {
        \\switch (self.state) {
    ;
    const parseEnd =
        \\            else => unreachable,
        \\        }
        \\    }
        \\
        \\    return if (!self.did_fail) blk: {
        \\        const infer_result = self.infer() catch |err| 
        \\            if (err == error.OutOfMemory)
        \\                 return error.OutOfMemory 
        \\            else ParseReturn{ 
        \\                .infer_fail = .{ .err = err, .msg = "action execution error" }, 
        \\        };
        \\        break :blk infer_result;
        \\    } else 
        \\        .{ .parse_fail = self.getParseError() };
        \\}
    ;
    const parseEndAccept =
        \\            else => unreachable,
        \\        }
        \\    }
        \\    return if (!self.did_fail) 
        \\        .pass
        \\    else 
        \\        .{ .parse_fail = self.getParseError() };
        \\}
    ;

    try self.writer.print("{s}\n", .{parseStart});
    for (self.ir.blocks.items) |blk| {
        try self.parseBlock(blk);
    }

    try self.writer.print("{s}\n", .{if (self.ir.is_acceptor)
        parseEndAccept
    else
        parseEnd});
}

fn parseBlock(self: *CodeGen, block: *lir.Block) !void {
    const writer = self.writer;
    try writer.print("{d} => {{\n", .{block.id});

    for (block.insts.items) |*instr| {
        try self.parseInstr(instr, block.fail);
    }

    try writer.print("}},\n", .{});
}

fn parseInstr(self: *CodeGen, instr: *lir.Instr, fail: ?*lir.Block) !void {
    const w = self.writer;
    switch (instr.tag) {
        .JMP => {
            try self.writer.print("self.state = {d};\n", .{instr.data.jmp.id});
        },
        .STRING => try self.parseString(instr, fail.?),
        .MATCH => try self.parseMatch(instr, fail.?),
        .TERM, .NONTERM => {
            try w.print("try self.parseNonterminal({d}, {}, {});\n", .{
                instr.data.ctx_jmp.next.id,
                instr.meta.neg,
                !instr.meta.pos and !instr.meta.neg,
            });

            if (instr.tag == .TERM) {
                try w.print("self.has_term_accept = false;\n", .{});
            }
        },
        .RET => try w.print("try self.returnFromNonterminal(false, false);\n", .{}),
        .EXIT_PASS => {
            try w.print("try self.returnFromNonterminal(false, false);\n", .{});
            try w.print("break;\n", .{});
        },
        .PRE_ACCEPT => {
            try w.print("self.has_term_accept = true;\n", .{});
            try w.print("self.term_end_state = self.state;\n", .{});
            try w.print("self.term_accept_len = self.acc;\n", .{});
        },
        .FAIL => try w.print("try self.returnFromNonterminal(true, false);\n", .{}),
        .EXIT_FAIL => {
            try w.print("try self.returnFromNonterminal(true, false);\n", .{});
            try w.print("break;\n", .{});
        },
        .TERMINAL_FAIL => {
            try w.print("if (self.has_term_accept) {{\n", .{});
            try w.print("    self.acc = self.term_accept_len;\n", .{});
            try w.print("    self.state = self.term_end_state;\n", .{});
            try w.print("    try self.returnFromNonterminal(false, false);\n", .{});
            try w.print("}} else {{\n", .{});
            try w.print("    try self.returnFromNonterminal(true, false);\n", .{});
            try w.print("}}\n", .{});
        },
    }
}

fn parseMatch(self: *CodeGen, instr: *lir.Instr, fail: *lir.Block) !void {
    const allocator = self.ir.allocator;
    const match = instr.data.match.items;
    try self.writer.print("switch (self.chars[self.acc + self.start]) {{\n", .{});

    var merged = try match[0].clone();
    defer merged.deinit();

    try self.parseProng(match[0], false);
    for (match[1..]) |prong| {
        try self.parseProng(prong, false);
        assert(!prong.isOverlapping(merged));
        const new_merge = try merged.mergeUnsafe(prong, allocator);
        merged.deinit();
        merged = new_merge;
    }

    try merged.invert();
    merged.consuming = false;
    merged.dest = fail;
    try self.parseProng(merged, true);

    try self.writer.print("}}\n", .{});
}

fn parseProng(self: *CodeGen, prong: lir.MatchProng, fail_prong: bool) !void {
    if (prong.labels.items.len == 0) return;
    for (prong.labels.items) |range| {
        try self.parseRange(range);
        try self.writer.print(",\n", .{});
    }

    try self.writer.print("=> {{\nself.state = {d};\n", .{prong.dest.id});

    // prong.consuming => !fail_prong
    assert(!prong.consuming or !fail_prong);
    if (prong.consuming) {
        try self.writer.print("self.acc += 1;\n", .{});
    } else if (fail_prong) {
        try self.writer.print("self.acc = 0;\n", .{});
        try self.writer.print(
            "self.infer_actions.shrinkRetainingCapacity(self.stack_start);\n",
            .{},
        );
        try self.writer.print("continue;\n", .{});
    }

    try self.writer.print("}},\n", .{});
}

fn parseRange(self: *CodeGen, range: lir.Range) !void {
    try su.writeChar(self.writer, range.from);
    if (range.isChar()) return;

    try self.writer.print("...", .{});
    try su.writeChar(self.writer, range.to);
}

fn parseString(self: *CodeGen, instr: *lir.Instr, fail: *lir.Block) !void {
    const w = self.writer;
    const lit = instr.data.str;
    try w.print("if (self.chars.len >= self.start + {d} + self.acc and\n", .{lit.len});
    try w.print("{s}std.mem.eql(u8, ", .{if (instr.meta.neg) "!" else ""});
    try su.writeString(self.writer, lit);
    try w.print(
        ", self\n.chars[self.start + self.acc..self.start + self.acc + {d}])){{\n",
        .{lit.len},
    );
    if (!instr.meta.neg and !instr.meta.pos) {
        try w.print("self.acc += {d};\n", .{lit.len});
    }
    try w.print("}} else {{\n", .{});
    try w.print("self.state = {d};\nself.acc = 0;\n", .{fail.id});
    try w.print("self.infer_actions.shrinkRetainingCapacity(self.stack_start);\n", .{});
    try w.print("continue;\n", .{});
    try self.writer.print("}}\n", .{});
}

fn callAction(self: *CodeGen) !void {
    const start =
        \\fn callAction(self: *@This(), state: usize, start: usize, length: usize) !void {
        \\    _ = start + length + self.state; // just to be able to compile 
        \\
        \\    switch (state) {
    ;
    const end =
        \\        else => unreachable,
        \\    }
        \\}
    ;
    try self.writer.print("{s}\n", .{start});

    for (self.ir.actions.items) |act| {
        try self.blockActionExec(act);
    }

    try self.writer.print("{s}\n", .{end});
}

fn blockActionExec(self: *CodeGen, action: lir.Action) !void {
    try self.writer.print("{d} => {{\n", .{action.base.id});

    if (!action.isEmpty()) {
        try self.actionExec(action);
    }

    try self.writer.print("}},\n", .{});
}

fn actionReturnExec(self: *CodeGen, rets: []const lir.LabeledRet) !void {
    for (0..rets.len) |i| {
        const ret = rets[rets.len - 1 - i];
        if (ret.ret.isNone()) continue;
        try self.writer.print(
            "const @\"${d}\" = self.calc_stack.pop().type_{d};\n",
            .{ ret.lbl, ret.place.base },
        );

        if (ret.needs_mut) {
            try self.writer.print(
                "var @\"$*{d}\" = @\"${d}\";\n",
                .{ ret.lbl, ret.lbl },
            );
        }
    }
}

fn actionBaseExec(self: *CodeGen, base: pir.ActionBase) !void {
    const ret = base.id == 0;
    const returns = !base.return_type.isNone();

    if (ret and returns) {
        try self.writer.print(
            "const @\"$ret\": {s} = ",
            .{base.return_type},
        );
    } else if (returns) {
        try self.writer.print(
            "const @\"${d}\": {s} = ",
            .{ base.id - 1, base.return_type },
        );
    }

    // write the action
    var i: usize = 0;
    while (base.getNext(i)) |action_var| {
        const end =
            @intFromPtr(action_var.var_decl.ptr) -
            @intFromPtr(base.data.ptr);

        try self.writer.print("{s}", .{base.data[i..end]});

        if (action_var.isEscape()) {
            try self.writer.print("{s}", .{action_var.var_decl[1..]});
        } else if (action_var.arg_num) |num| {
            try self.writer.print(
                "@\"${s}{d}\"",
                .{ if (action_var.escape) "*" else "", num },
            );
        } else {
            assert(action_var.isMatchedString());
            try self.writer.print(
                "self.chars[start .. start + length]",
                .{},
            );
        }

        i = end + action_var.var_decl.len;
    }
    try self.writer.print("{s}", .{base.data[i..]});

    if (!returns) return;
    try self.writer.print(";\n", .{});

    if (!base.mut) return;
    try self.writer.print(
        "var \"$*{d}\" = @\"${d}\"",
        .{ base.id - 1, base.id - 1 },
    );
}

fn actionExec(self: *CodeGen, action: lir.Action) !void {
    const rets = action.args.items;
    try self.actionReturnExec(rets);

    // check that the action is an expression or not
    const bases = action.bases.items;
    for (0..bases.len) |i| {
        const base = bases[bases.len - 1 - i];
        try self.actionBaseExec(base);
    }

    // add the return value to the calc stack
    const last_base = bases[0];
    const returns = !last_base.return_type.isNone();
    if (!returns) return;
    try self.writer.print(
        "try self.calc_stack.append(.{{.type_{d} = @\"$ret\"}});\n",
        .{action.base.base},
    );
}

fn infer(self: *CodeGen) !void {
    const end =
        \\    try self.finalizeInferInstrs();
        \\    for (self.infer_instrs.items) |instr| {{
        \\        try self.callAction(instr.state, instr.start, instr.length);
        \\    }}
        \\
        \\    const output: ParseReturn = if (self.did_fail) 
        \\        .{{ .infer_fail = .{{ 
        \\            .err = self.infer_fail_err, 
        \\            .msg = self.infer_fail_msg, 
        \\    }} }} else
        \\        .{{ .pass = {s} }};
        \\
        \\    assert(self.did_fail or self.calc_stack.items.len == 0);
        \\    return output;
        \\}}
    ;

    try self.writer.print(
        "fn infer(self: *@This()) !ParseReturn {{\n",
        .{},
    );
    try self.writer.print(end, .{
        if (self.parse_return.isNone())
            "void{}"
        else
            "self.calc_stack.pop().type_0",
    });
}

fn memoInfer(self: *CodeGen) !void {
    const inferStart =
        \\fn memoInfer(self: *@This(), state: State, start: CharIdx) Allocator.Error!void {
        \\self.infer_start = start;
        \\self.infer_acc = start;
        \\self.infer_state = ActionTranslate[state];
        \\self.infer_done = false;
        \\
        \\while (!self.infer_done) {
        \\    switch (self.infer_state) {
    ;
    const inferEnd =
        \\        else => unreachable,
        \\    }
        \\}
        \\}
    ;
    try self.writer.print("{s}\n", .{inferStart});

    var count: usize = 0;
    for (self.ir.actions.items) |action| {
        try self.inferAction(action, &count);
    }

    try self.writer.print("{s}\n", .{inferEnd});
}

fn inferAction(self: *CodeGen, action: lir.Action, count: *usize) !void {
    for (action.nonterms.items) |nonterm| {
        const blk = nonterm.calls;
        const add = blk.id;
        const skip = !blk.meta.moves_actions and !blk.meta.has_actions;

        if (skip) {
            try self.inferCall(count, nonterm.consumes, add, true, false);
        } else {
            try self.inferCall(count, nonterm.consumes, add, false, false);
        }
    }

    try self.inferCall(count, action.last_offset, action.base.id, false, true);
}

fn inferCall(
    self: *CodeGen,
    count: *usize,
    offset: usize,
    add: usize,
    comptime skip: bool,
    comptime ret: bool,
) !void {
    try self.writer.print("{d} => {{\n", .{count.*});
    try self.writer.print("self.infer_acc += {d};\n", .{offset});
    try self.writer.print(if (ret)
        "try self.returnFromInfer({d});\n"
    else if (skip)
        "self.skipInferNonterminal({d});\n"
    else
        "try self.inferNonterminal({d});\n", .{add});
    try self.writer.print("}},\n", .{});
    count.* += 1;
}
