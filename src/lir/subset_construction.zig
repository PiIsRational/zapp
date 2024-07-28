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
look_refs: ?*std.AutoHashMap(ra.ExecPlace, u32) = null,
automata: ?*std.ArrayList(*ra.Automaton) = null,

/// takes in the first block elegible to become a dfa and
/// returns dfas connected witch each other using lookahead calls
pub const Automatizer = struct {
    look_refs: std.AutoHashMap(ra.ExecPlace, u32),
    automata: std.ArrayList(*ra.Automaton),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Automatizer {
        return .{
            .allocator = allocator,
            .look_refs = std.AutoHashMap(ra.ExecPlace, u32).init(allocator),
            .automata = std.ArrayList(ra.Automaton).init(allocator),
        };
    }

    pub fn genLookAutomata(
        self: *Automatizer,
        start_block: *ir.Block,
    ) !std.ArrayList(ra.Automaton) {
        var sub_generator = initTable(self.allocator, &self.look_refs, &self.automata);
        defer sub_generator.deinit();

        try self.look_refs.put(.{ .block_id = start_block.id }, 0);
        try self.automata.append(&sub_generator.dfa);

        return sub_generator.genDfa(start_block);
    }

    pub fn deinit(self: Automatizer) void {
        self.look_refs.deinit();
    }
};

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

fn initTable(
    allocator: Allocator,
    look_refs: *std.AutoHashMap(ra.ExecPlace, u32),
    automata: *std.ArrayList(*ra.Automaton),
) DfaGen {
    var self = init(allocator);
    self.look_refs = look_refs;
    self.automata = automata;

    return self;
}

/// generates a Dfa (automaton with only match and ret/fail)
pub fn genDfa(self: *DfaGen, start_block: *ir.Block) !ra.Automaton {
    var look_buf = std.ArrayList(ra.ExecState.SplitOffResult).init(self.allocator);
    defer look_buf.deinit();

    var start_state = try DfaState.init(self.allocator, start_block, &look_buf);
    try self.dfa_states.append(start_state);
    try self.put(try start_state.toKey(&self.key_scratch), try self.dfa.getNew());
    look_buf.clearRetainingCapacity();

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
                try split_state.goEps(&look_buf);
                defer look_buf.clearRetainingCapacity();

                var labels = std.ArrayList(ir.Range).init(self.allocator);
                var iter = prong.set.rangeIter();
                while (iter.next()) |range| try labels.append(range);

                const dst_blk = try self.getBlock(&split_state, &look_buf);

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

fn getBlock(
    self: *DfaGen,
    state: *DfaState,
    buf: *std.ArrayList(ra.ExecState.SplitOffResult),
) !*ir.Block {
    const SplitOffResult = ra.ExecState.SplitOffResult;
    if (buf.items.len >= 1) {
        std.sort.pdq(SplitOffResult, buf.items, {}, SplitOffResult.lessThan);

        var count: usize = 0;
        for (buf.items[1..]) |item| if (!buf.items[count].eql(item)) {
            buf.items[count] = item;
            count += 1;
        };
        buf.shrinkRetainingCapacity(count);
    }

    const semi_empty = state.isSemiEmpty();
    const empty = !semi_empty and state.isEmpty();
    if (semi_empty) state.removeEmpty();

    const key = try state.toKey(&self.key_scratch);
    defer key.deinit(self.allocator);

    if (self.map.get(key)) |block| {
        return block;
    }

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
    return dst_blk;
}

fn genLooks(
    self: *DfaGen,
    base_block: *ir.Block,
    buf: *std.ArrayList(ra.ExecState.SplitOffResult),
) !*ir.Block {
    assert(base_block.insts.items.len == 0);
    if (buf.items.len == 0) return base_block;
    var curr_block = base_block;

    for (buf.items) |off_split| {
        const sub_automaton = try self.getSubAutomaton(off_split);

        try curr_block.insts.append(ir.Instr.initNonterm(
            sub_automaton.start,
            base_block,
            ir.InstrMeta.initLookahead(off_split.look),
        ));
        const new_block = try self.dfa.getNew();
        curr_block.fail = new_block;
        curr_block = new_block;
    }

    curr_block.fail = null;
    try curr_block.insts.append(ir.Instr.initTag(.TERM_FAIL));
}

fn getSubAutomaton(self: *DfaGen, off_split: ra.ExecState.SplitOffResult) void {
    const automata = self.automata.?.items;
    if (self.look_refs.?.get(off_split.toExecPlace())) |at_key| {
        return automata[at_key];
    }

    const insts = off_split.blk.insts.items;
    const instr = insts[off_split.instr];
    const new_at = if (instr.tag == .NONTERM) blk: {
        var sub_generator = initTable(
            self.allocator,
            self.look_refs.?,
            self.automata.?,
        );

        defer sub_generator.deinit();
        const block = instr.data.ctx_jmp.dest;
        try self.look_refs.?.putNoClobber(.{ .block_id = block.id }, automata.len);
        break :blk sub_generator.genDfa(block);
    } else blk: {
        try self.look_refs.?.putNoClobber(off_split.toExecPlace(), automata.len);
        break :blk try ra.Automaton.initInstr(self.allocator, instr);
    };

    try self.automata.?.append(new_at);
    return new_at;
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

    pub fn init(
        allocator: Allocator,
        block: *ir.Block,
        buf: *std.ArrayList(ra.ExecState.SplitOffResult),
    ) !DfaState {
        return try initFromExec(allocator, try ra.ExecState.init(allocator, block), buf);
    }

    pub fn initFromExec(
        allocator: Allocator,
        exec: ra.ExecState,
        buf: *std.ArrayList(ra.ExecState.SplitOffResult),
    ) !DfaState {
        var sub_states = std.ArrayList(ra.ExecState).init(allocator);
        try sub_states.append(exec);

        var self: DfaState = .{
            .sub_states = sub_states,
            .action = null,
        };

        try self.goEps(buf);
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

    pub fn goEps(
        self: *DfaState,
        buf: *std.ArrayList(ra.ExecState.SplitOffResult),
    ) !void {
        if (self.isEmpty()) return;

        while (try self.fillBranches() or try self.execJumps(false, buf)) {}

        if (self.isSemiEmpty()) {
            assert(self.sub_states.items.len > 0);
            self.action = null;
            for (self.sub_states.items) |sub| {
                if (sub.blocks.items.len == 0) {
                    self.action = sub.last_action;
                    break;
                }

                const instr = sub.getCurrInstr().?;
                if (instr.tag != .PRE_ACCEPT) continue;

                self.action = instr.data.action;
                break;
            }
            assert(self.action != null);
        }

        while (try self.fillBranches() or try self.execJumps(true, buf)) {}

        self.resetHadFill();

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
    fn deduplicate(self: *DfaState, key: *DfaState.Key) void {
        if (self.sub_states.items.len <= 1) return;

        var i: usize = 1;
        const subs = self.sub_states.items;
        var len = subs.len;
        while (i <= len) : (i += 1) {
            if (subs[i - 1].blocks.items.len != 0) continue;
            len -= 1;
            i -= 1;
            std.mem.swap(ra.ExecState, &subs[i], &subs[len]);
        }

        assert(len == key.sub_states.len);

        const keys = key.sub_states;
        self.reorder(keys);

        i = 1;
        var idx: usize = 1;
        // normal states
        while (i < len) : (i += 1) {
            const curr = &self.sub_states.items[i];

            if (keys[i - 1].eql(keys[i])) {
                curr.deinit();
                continue;
            }

            self.sub_states.items[idx] = curr.*;
            idx += 1;
        }

        // terminal states
        while (i < self.sub_states.items.len) : (i += 1) {
            const curr = &self.sub_states.items[i];
            const last = self.sub_states.items[idx - 1];

            if (i != len and curr.eql(last)) {
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
    fn reorder(self: *DfaState, keys: []ra.ExecState.Key) void {
        const BaseCtx = struct {
            states: []ra.ExecState,
            keys: []ra.ExecState.Key,

            pub fn lessThan(ctx: @This(), lhs: usize, rhs: usize) bool {
                const l_k = ctx.keys[lhs];
                const r_k = ctx.keys[rhs];

                return l_k.lessThan(r_k);
            }

            pub fn swap(ctx: @This(), lhs: usize, rhs: usize) void {
                std.mem.swap(ra.ExecState, &ctx.states[lhs], &ctx.states[rhs]);
                std.mem.swap(ra.ExecState.Key, &ctx.keys[lhs], &ctx.keys[rhs]);
            }
        };

        const TermCtx = struct {
            pub fn less(_: void, lhs: ra.ExecState, rhs: ra.ExecState) bool {
                assert(lhs.blocks.items.len == 0 and rhs.blocks.items.len == 0);
                return lhs.last_action < rhs.last_action;
            }
        };

        std.sort.pdqContext(0, keys.len, BaseCtx{
            .states = self.sub_states.items,
            .keys = keys,
        });

        std.sort.pdq(ra.ExecState, self.sub_states.items[keys.len..], void{}, TermCtx.less);
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

    fn execJumps(
        self: *DfaState,
        skip_accept: bool,
        call_buf: *std.ArrayList(ra.ExecState.SplitOffResult),
    ) !bool {
        var had_change = false;
        for (self.sub_states.items) |*sub| {
            if (skip_accept) {
                had_change = sub.skipPreAccept() or had_change;
            }

            const jmp_result = try sub.execJumps();
            had_change = jmp_result == .CHANGE or had_change;

            const instr = sub.getCurrInstr() orelse continue;
            if (jmp_result != .LOOKAHEAD or instr.meta.isConsuming()) continue;

            try call_buf.append(sub.splitOff());
            had_change = true;
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
        sub_states: []ra.ExecState.Key,
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
                    try writer.print("(∅ , {d}) ", .{act});
                } else {
                    try writer.print("(∅ ) ", .{});
                }
            }

            try writer.print("{{ ", .{});
            for (self.sub_states) |sub_key| {
                try writer.print("{s} ", .{sub_key});
            }
            try writer.print("}}", .{});
        }
    };

    pub fn toKey(self: *DfaState, scratch: *std.AutoHashMap(usize, usize)) !Key {
        var k = try self.toKeyNoDedupe(scratch);
        defer k.deinit(scratch.allocator);
        self.deduplicate(&k);

        return try self.toKeyNoDedupe(scratch);
    }

    pub fn toKeyNoDedupe(self: *DfaState, scratch: *std.AutoHashMap(usize, usize)) !Key {
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
                try writer.print("(∅ , {d}) ", .{act});
            } else {
                try writer.print("(∅ ) ", .{});
            }
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
