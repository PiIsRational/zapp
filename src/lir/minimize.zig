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

const StateSet = std.ArrayList(ra.ExecState);

/// minimization keeps `PRE_ACCEPT` and `RET` apart
pub fn minimize(allocator: Allocator, automaton: ra.Automaton) !ra.Automaton {
    assert(automaton.isDfa());

    var sets = std.ArrayList(StateSet).init(allocator);
    try sets.append(undefined);
    defer {
        for (sets.items) |set| {
            for (set.items) |state| {
                state.deinit();
            }
            set.deinit();
        }
        sets.deinit();
    }
    var buf = std.ArrayList(StateSet).init(allocator);
    defer buf.deinit();
    var lookup = try allocator.alloc(usize, automaton.blocks.items.len);
    defer allocator.free(lookup);

    var max_action: usize = 0;
    for (automaton.blocks.items) |blk| {
        const insts = blk.insts.items;
        if (insts[0].tag != .RET and insts[0].tag != .PRE_ACCEPT) continue;
        max_action = @max(max_action, insts[0].data.action);
    }
    var action_lookup = try allocator.alloc(usize, max_action + 1);
    @memset(action_lookup, 0);
    defer allocator.free(action_lookup);

    const fail_state = try ra.ExecState.init(allocator, automaton.fail);
    defer fail_state.deinit();

    var base_states = StateSet.init(allocator);
    for (automaton.blocks.items, 0..) |block, i| {
        var new_state = try ra.ExecState.init(allocator, block);
        _ = new_state.skipPreAccept();
        const insts = block.insts.items;

        if (insts[0].tag != .RET and
            block != automaton.fail and
            insts[0].tag != .PRE_ACCEPT)
        {
            try base_states.append(new_state);
            lookup[i] = 0;
            continue;
        }

        if (block == automaton.fail or insts[0].tag == .RET) {
            var accepting = StateSet.init(allocator);
            try accepting.append(new_state);
            try sets.append(accepting);

            lookup[i] = sets.items.len;
            continue;
        }

        const action_set = action_lookup[insts[0].data.action];
        if (action_set != 0) {
            const accepting = &sets.items[action_set - 1];
            try accepting.append(new_state);
            lookup[i] = action_set;
            continue;
        }

        var accepting = StateSet.init(allocator);
        try accepting.append(new_state);
        try sets.append(accepting);

        lookup[i] = sets.items.len;
        action_lookup[insts[0].data.action] = sets.items.len;
    }

    sets.items[0] = base_states;

    var count: usize = 0;
    while (count != sets.items.len) {
        count = sets.items.len;
        while (sets.popOrNull()) |set| {
            assert(set.items.len > 0);
            if (set.items.len == 1) {
                try buf.append(set);
                continue;
            }

            defer set.deinit();

            var splits = try getSplits(set);
            defer splits.deinit();
            const len = buf.items.len;

            for (splits.items) |split| {
                const moved = try splitOn(set, split, fail_state);
                defer {
                    for (moved.items) |item| item.deinit();
                    moved.deinit();
                }

                try splitUp(set, moved, lookup, &buf);

                if (buf.items.len > len + 1) break;

                const new_set = buf.pop();
                new_set.deinit();
            }

            if (buf.items.len == len) try buf.append(try set.clone());
        }

        try sets.appendSlice(buf.items);
        renewLookup(lookup, sets);
        buf.clearRetainingCapacity();
    }

    var nerode_dfa = ra.Automaton.init(allocator);
    for (sets.items) |_| _ = try nerode_dfa.getNew();
    for (sets.items, nerode_dfa.blocks.items) |set, new_block| {
        var repr = set.items[0];
        repr.instr = 0;

        while (repr.getCurrInstrOrNull()) |equivalent| : (repr.instr += 1) {
            switch (equivalent.tag) {
                .MATCH => {
                    for (set.items) |state| {
                        if (state.blocks.getLast() != automaton.start) continue;
                        nerode_dfa.start = new_block;
                    }

                    var instr: ir.Instr = .{
                        .tag = .MATCH,
                        .meta = ir.InstrMeta.Empty,
                        .data = .{
                            .match = std.ArrayList(ir.MatchProng).init(allocator),
                        },
                    };

                    for (equivalent.data.match.items) |prong| {
                        var new_prong = try prong.clone();
                        new_prong.dest = nerode_dfa.blocks.items[lookup[prong.dest.id]];
                        try instr.data.match.append(new_prong);
                    }
                    try new_block.insts.append(instr);
                },
                .TERM_FAIL,
                => {
                    try new_block.insts.append(equivalent);
                    nerode_dfa.fail = new_block;
                },
                .RET => try new_block.insts.append(equivalent),
                .PRE_ACCEPT => try new_block.insts.append(equivalent),
                else => unreachable,
            }
        }
    }

    try ra.mergeDest(allocator, &nerode_dfa);
    return nerode_dfa;
}

fn cloneSet(set: StateSet) !StateSet {
    var new = StateSet.init(set.allocator);
    try new.ensureTotalCapacity(set.items.len);
    for (set.items) |item| try new.append(try item.clone());

    return new;
}

fn renewLookup(lookup: []usize, buf: std.ArrayList(StateSet)) void {
    for (buf.items, 0..) |set, i| for (set.items) |state| {
        const id = state.blocks.getLast().id;
        lookup[id] = i;
    };
}

fn splitUp(
    set: StateSet,
    moved_set: StateSet,
    lookup: []const usize,
    buf: *std.ArrayList(StateSet),
) !void {
    assert(set.items.len > 1);
    assert(moved_set.items.len == set.items.len);
    const Ctx = struct {
        lookup: []const usize,
        set: []ra.ExecState,
        moved: []ra.ExecState,

        pub fn lessThan(self: @This(), lhs: usize, rhs: usize) bool {
            const lid = self.moved[lhs].blocks.getLast().id;
            const rid = self.moved[rhs].blocks.getLast().id;

            return self.lookup[lid] < self.lookup[rid];
        }

        pub fn swap(self: @This(), a: usize, b: usize) void {
            std.mem.swap(ra.ExecState, &self.set[a], &self.set[b]);
            std.mem.swap(ra.ExecState, &self.moved[a], &self.moved[b]);
        }
    };

    const ctx = Ctx{ .set = set.items, .moved = moved_set.items, .lookup = lookup };
    std.sort.pdqContext(0, set.items.len, ctx);

    var curr_set: StateSet = StateSet.init(set.allocator);
    try curr_set.append(set.items[0]);

    for (
        set.items[1..],
        moved_set.items[0 .. set.items.len - 1],
        moved_set.items[1..],
    ) |val, last, curr| {
        const lid = last.blocks.getLast().id;
        const cid = curr.blocks.getLast().id;

        if (lookup[lid] == lookup[cid]) {
            try curr_set.append(val);
            continue;
        }

        try buf.append(curr_set);
        curr_set = StateSet.init(set.allocator);
        try curr_set.append(val);
    }

    try buf.append(curr_set);
}

fn splitOn(set: StateSet, branch: ra.SplitBranch, fail: ra.ExecState) !StateSet {
    assert(set.items.len > 0);
    var new = StateSet.init(set.allocator);
    try new.ensureTotalCapacity(set.items.len);

    for (set.items) |item| {
        const instr = item.getCurrInstr();
        if (instr == null or instr.?.tag == .RET) {
            try new.append(try item.clone());
            continue;
        }

        var split_result = try item.splitOn(branch.set);
        if (split_result) |*result| _ = result.skipPreAccept();
        try new.append(split_result orelse try fail.clone());
    }

    return new;
}

fn getSplits(set: StateSet) !std.ArrayList(ra.SplitBranch) {
    var prongs = std.ArrayList(ra.SplitBranch)
        .init(set.allocator);

    for (set.items) |*state| try state.addBranches(&prongs, true);
    _ = try ra.canonicalizeBranches(&prongs);

    return prongs;
}

fn printSet(set: StateSet) void {
    if (set.items.len == 0) {
        std.debug.print("∅ \n", .{});
        return;
    }

    std.debug.print("{{ ", .{});
    for (set.items, 0..) |it, i| {
        std.debug.print("{s}", .{it});
        if (i + 1 == set.items.len) break;
        std.debug.print(", ", .{});
    }

    std.debug.print(" }}\n", .{});
}
