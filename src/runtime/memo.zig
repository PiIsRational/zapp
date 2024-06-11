const MemoConfig = struct {
    rule_count: usize,
    chunks: ?usize = null,
};

const MemoIndex = enum(u32) {
    const stdlib = @import("std");
    empty = stdlib.math.maxInt(u32),
    _,

    pub fn init(value: u32) MemoIndex {
        return @enumFromInt(value);
    }

    pub fn unwrap(self: @This()) ?u32 {
        if (self == .empty) return null;
        return @intFromEnum(self);
    }
};

fn UnorderedMemo(comptime T: type, comptime size: usize) type {
    const stdlib = @import("std");

    if (size * @sizeOf(T) < @sizeOf(MemoIndex)) {
        @compileError("too small chunks!");
    }

    return struct {
        const Cell = struct {
            table: [size]T align(@alignOf(MemoIndex)),

            pub fn next(self: *@This()) *MemoIndex {
                return @ptrCast(&self.table);
            }
        };

        free_list: MemoIndex,
        values: stdlib.ArrayList(Cell),

        pub fn init(allocator: stdlib.mem.Allocator) @This() {
            return .{
                .free_list = .empty,
                .values = stdlib.ArrayList(Cell).init(allocator),
            };
        }

        pub fn new(self: *@This(), comptime default: T) !MemoIndex {
            const cell_default: Cell = .{ .table = [1]T{default} ** size };
            if (self.free_list.unwrap()) |value| {
                const index = self.free_list;
                self.free_list = self.values.items[value].next().*;
                self.values.items[index.unwrap() orelse unreachable] = cell_default;
                return index;
            }

            try self.values.append(cell_default);
            return MemoIndex.init(@intCast(self.values.items.len - 1));
        }

        pub fn free(self: *@This(), index: MemoIndex) void {
            const value = index.unwrap() orelse unreachable;
            self.values.items[value].next().* = self.free_list;
            self.free_list = index;
        }

        pub fn freeAll(self: *@This()) void {
            self.free_list = .empty;
            self.values.clearRetainingCapacity();
        }

        pub fn getMemUseage(self: @This()) usize {
            return self.values.capacity * @sizeOf(Cell);
        }

        pub fn get(self: @This(), index: MemoIndex, rule: usize) *T {
            return &self.values.items[index.unwrap() orelse unreachable].table[rule];
        }

        pub fn deinit(self: *@This()) void {
            self.values.deinit();
        }
    };
}

fn MemoTable(comptime config: MemoConfig) type {
    return struct {
        const stdlib = @import("std");
        const Chunks = if (config.chunks) |chunks| chunks else chunksFromRules();
        const SubMemoSize = @divFloor(config.rule_count, Chunks) +
            if (@rem(config.rule_count, Chunks) == 0) 0 else 1;
        const LengthSubMemo = UnorderedMemo(u32, SubMemoSize);
        const StateSubMemo = UnorderedMemo(u16, SubMemoSize);

        start: usize,
        max_chars: usize,
        base_table: stdlib.ArrayList([Chunks]MemoIndex),
        lengths: [Chunks]LengthSubMemo,
        states: [Chunks]StateSubMemo,

        fn chunksFromRules() usize {
            return stdlib.math.sqrt(config.rule_count);
        }

        pub const MemoResult = union(enum) {
            pass: struct {
                length: u32,
                end_state: u16,
            },
            fail: struct {
                end_state: u16,
            },
            empty: void,
        };

        pub fn init(allocator: stdlib.mem.Allocator) @This() {
            var states = [1]StateSubMemo{undefined} ** Chunks;
            var lengths = [1]LengthSubMemo{undefined} ** Chunks;
            for (&states, &lengths) |*state, *length| {
                state.* = StateSubMemo.init(allocator);
                length.* = LengthSubMemo.init(allocator);
            }

            return .{
                .max_chars = 0,
                .start = 0,
                .base_table = stdlib.ArrayList([Chunks]MemoIndex).init(allocator),
                .states = states,
                .lengths = lengths,
            };
        }

        pub fn reset(self: *@This()) void {
            self.max_chars = 0;
            self.start = 0;
            self.base_table.clearRetainingCapacity();

            for (&self.states, &self.lengths) |*state, *length| {
                state.freeAll();
                length.freeAll();
            }
        }

        pub fn deinit(self: *@This()) void {
            self.base_table.deinit();

            for (&self.states, &self.lengths) |*state, *length| {
                state.deinit();
                length.deinit();
            }
        }

        fn assumeSize(self: *@This(), chars: usize, rule: usize) !void {
            const first_level_index = @divFloor(rule, SubMemoSize);
            const lookup_place = chars - self.start;
            if (chars >= self.base_table.items.len + self.start) {
                const append_size = chars + 1 - self.base_table.items.len - self.start;
                try self.base_table.appendNTimes([1]MemoIndex{.empty} ** Chunks, append_size);
            } else if (self.base_table.items[lookup_place][first_level_index] != .empty)
                return;

            const state_idx = try self.states[first_level_index].new(0);
            const length_idx = try self.lengths[first_level_index].new(0);
            stdlib.debug.assert(state_idx != .empty and state_idx == length_idx);

            self.base_table.items[lookup_place][first_level_index] = length_idx;
        }

        fn getTableIdx(self: @This(), char: usize, rule: usize) MemoIndex {
            const lookup_place = char - self.start;
            const first_level_index = @divFloor(rule, SubMemoSize);

            return self.base_table.items[lookup_place][first_level_index];
        }

        fn getPtrs(self: *@This(), chars: usize, rule: usize) struct { state: *u16, length: *u32 } {
            const first_level_index = @divFloor(rule, SubMemoSize);
            const second_level_index = @rem(rule, SubMemoSize);
            const table_index = self.getTableIdx(chars, rule);
            stdlib.debug.assert(table_index != .empty);

            return .{
                .state = self.states[first_level_index].get(table_index, second_level_index),
                .length = self.lengths[first_level_index].get(table_index, second_level_index),
            };
        }

        pub fn add(self: *@This(), char: usize, rule: usize, value: MemoResult) !void {
            if (char < self.start) return;
            try self.assumeSize(char, rule);
            const ptrs = self.getPtrs(char, rule);

            switch (value) {
                .pass => |val| {
                    ptrs.length.* = val.length + 2;
                    ptrs.state.* = val.end_state;
                },
                .fail => |val| {
                    ptrs.length.* = 1;
                    ptrs.state.* = val.end_state;
                },
                .empty => unreachable,
            }
        }

        pub fn get(self: *@This(), char: usize, rule: usize) MemoResult {
            if (char < self.start) return .empty;
            if (char >= self.start + self.base_table.items.len) return .empty;
            if (self.getTableIdx(char, rule) == .empty) return .empty;

            const ptrs = self.getPtrs(char, rule);
            const value = ptrs.length.*;
            if (value == 0) return .empty;

            if (value == 1) return .{
                .fail = .{ .end_state = ptrs.state.* },
            };

            return .{ .pass = .{
                .end_state = ptrs.state.*,
                .length = value - 2,
            } };
        }

        pub fn getState(self: *@This(), char: usize, rule: usize) u32 {
            const ptrs = self.getPtrs(char, rule);
            return ptrs.state.*;
        }

        pub fn getMemUseage(self: *@This()) usize {
            var useage = self.base_table.capacity * @sizeOf(MemoIndex);
            for (self.states, self.lengths) |state, length| {
                useage += state.getMemUseage() + length.getMemUseage();
            }

            return useage;
        }

        fn invalidateOld(self: *@This(), new_start: usize) void {
            if (new_start < self.base_table.items.len) {
                for (self.base_table.items[0..new_start]) |sub| for (sub, 0..) |index, base_rule| {
                    if (index == .empty) continue;

                    self.states[base_rule].free(index);
                    self.lengths[base_rule].free(index);
                };
            } else {
                for (&self.lengths, &self.states) |*length, *state| {
                    length.freeAll();
                    state.freeAll();
                }
            }
        }

        pub fn resetTo(self: *@This(), new_begin: usize) void {
            self.max_chars = @max(self.max_chars, self.base_table.items.len);

            const new_start = new_begin - self.start;
            self.invalidateOld(new_start);

            if (new_start < self.base_table.items.len) {
                stdlib.mem.copyForwards(
                    [Chunks]MemoIndex,
                    self.base_table.items,
                    self.base_table.items[new_start..],
                );
                self.base_table.shrinkRetainingCapacity(
                    self.base_table.items.len - new_start,
                );
            } else {
                self.base_table.clearRetainingCapacity();
            }

            self.start = new_begin;
        }
    };
}
