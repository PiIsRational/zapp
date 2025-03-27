pub const imports =
    \\const std = @import("std");
    \\const Allocator = std.mem.Allocator;
    \\const assert = std.debug.assert;
;

pub const structs =
    \\
    \\const State = u16;
    \\const Rule = u16;
    \\const CharIdx = u32;
    \\const CharSpanLength = u32;
    \\
    \\/// the maximal accepted input size for the parser
    \\/// (the -2 is because 0 and 1 represent empty cells and not math cells)
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

pub const vars =
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
;

pub const funcs =
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
    \\}
    \\
    \\pub fn stats(self: *@This()) Stats {
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
    \\    const frame = self.stack.pop() orelse blk: {
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

pub const inferFuncs =
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
    \\fn returnFromInfer(self: *@This(), state: State) Allocator.Error!void {
    \\    const frame = self.infer_stack.pop() orelse blk: {
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

pub const parse_return =
    \\pub const ParseReturn = union(enum) {{
    \\    pass: {s},
    \\    parse_fail: ParsingError,
    \\    infer_fail: InferError,
    \\}};
    \\
;

pub const parseStart =
    \\pub fn parse(
    \\    self: *@This(), 
    \\    chars: [:0]const u8, 
    \\) ParserError!ParseReturn {
    \\
    \\if (chars.len > MaxInputSize) return error.InputTooLarge;
    \\
    \\self.reset(chars);
    \\
    \\while (true) {
    \\switch (self.state) {
;

pub const parseEnd =
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

pub const parseEndAccept =
    \\            else => unreachable,
    \\        }
    \\    }
    \\    return if (!self.did_fail) 
    \\        .pass
    \\    else 
    \\        .{ .parse_fail = self.getParseError() };
    \\}
;

pub const infer =
    \\fn infer(self: *@This()) !ParseReturn {{
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

pub const resetMemo =
    \\fn resetMemo(self: *@This(), new_begin: usize) Allocator.Error!void {{
    \\    {s}
    \\    if (new_begin < self.memo.start) {{
    \\        self.did_fail = true;
    \\        return;
    \\    }}
    \\    if (self.memo.base_table.items.len > self.memo.max_chars) 
    \\        self.max_memo_state = self.state;
    \\    self.memo.resetTo(new_begin);
    \\}}
;
