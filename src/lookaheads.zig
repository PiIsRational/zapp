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

    pub fn init(allocator: Allocator) LookaheadEmitter {
        return .{
            .allocator = allocator,
            .map = LookaheadMap.init(allocator),
            .states = std.ArrayList(LookaheadState).init(allocator),
            .map_keys = std.ArrayList(LookaheadState.Key).init(allocator),
        };
    }

    pub fn emit(self: *LookaheadEmitter, start: *ir.Block) !ra.Automaton {
        const nfa = ra.Automaton.init(self.allocator);

        _ = start;
        return nfa;
    }

    pub fn put(self: *LookaheadEmitter, key: LookaheadState.Key, value: *ir.Block) !void {
        try self.map_keys.append(key);
        try self.map.put(key, value);
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

        return result != .NO_CHANGE;
    }

    fn splitOff(self: *LookaheadState) !void {
        // no idea what is going ot happen
        const instr = self.base.getCurrInstr() orelse unreachable;
        switch (instr.tag) {
            .NONTERM => {},
            .STRING => {},
            .MATCH => {},
            else => unreachable,
        }
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
