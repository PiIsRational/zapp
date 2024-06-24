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

const LookaheadLifetime = enum {
    INSTR,
    STANDARD,
};

pub const LookaheadState = struct {
    base: ra.ExecState,
    look: LookaheadType,
    lifetime: LookaheadLifetime,
    sub_states: std.ArrayList(LookaheadState),

    pub const Key = struct {
        base: ra.ExecState.Key,
        look: LookaheadType,
        lifetime: LookaheadLifetime,
        sub_states: []const LookaheadState.Key,

        pub fn hash(self: Key, hasher: anytype) void {
            self.base.hash(hasher);

            assert(std.meta.hasUniqueRepresentation(LookaheadType));
            assert(std.meta.hasUniqueRepresentation(LookaheadLifetime));

            hasher.update(std.mem.asBytes(&self.look));
            hasher.update(std.mem.asBytes(&self.lifetime));

            for (self.sub_states) |sub| {
                assert(sub.look != .NONE); // sub states cannot have no lookahead
                sub.hash(hasher);
            }
        }

        pub fn deinit(self: Key, allocator: Allocator) void {
            self.base.deinit(allocator);
            for (self.sub_states) |sub| sub.deinit(allocator);
            allocator.free(self.sub_states);
        }
    };

    pub fn toKey(self: LookaheadState) Key {
        _ = self;
        unreachable;
    }

    pub fn deinit(self: LookaheadState) void {
        self.base.deinit();
        for (self.sub_states.items) |sub| sub.deinit();
        self.sub_states.deinit();
    }
};
