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
const DfaGen = @import("subset_construction.zig");
const minimize = @import("minimize.zig").minimize;
const LookaheadEmitter = @import("lookaheads.zig");
const gvgen = @import("../graphviz_gen.zig");

const PassManager = @This();

ir: *ir.LowIr,

const PassError = Allocator.Error;

// the plan would be to treat everything in two passes:
//
// * first try to remove lookahead and get a simple model for an nfa
// * second implement the nfa to dfa conversion (already implemented)
//
// it would be nice to have some minimization algorithms.
// there woule also be a benefit from refactoring some of the code to
// make all the analyses simpler and more reusable.

pub fn optimize(lir: *ir.LowIr) !void {
    var self: PassManager = .{
        .ir = lir,
    };

    try self.blockPass(addAutomaton);
}

fn addAutomaton(self: *PassManager, blk: *ir.Block) PassError!void {
    const stdout = std.io.getStdErr().writer();
    if (!blk.meta.is_target or !blk.meta.used_terminal) return;
    const allocator = self.ir.allocator;

    var emitter = LookaheadEmitter.init(allocator);
    defer emitter.deinit();
    var dfa_gen = DfaGen.init(allocator);
    defer dfa_gen.deinit();

    const nfa = try emitter.emit(blk);
    defer nfa.deinit(allocator);

    const dfa = try dfa_gen.genDfa(nfa.start);
    defer dfa.deinit(allocator);

    var nerode = try minimize(allocator, dfa);
    nerode.start.meta = .{
        .mid_recurse = false,
        .right_recurse = true, // may be false too
        .regular = true, // generally true
        .finite = false, // possibly true
        .is_terminal = true,
        .nonterm_fail = false, // ?
        .has_actions = true, // possibly false
        .moves_actions = true, // possibly false
        .is_target = true,
        .used_terminal = true,
    };

    try ra.compressMatches(allocator, &nerode);
    nerode.setFail();
    gvgen.genAutomaton(stdout, nfa, "d") catch unreachable;
    gvgen.genAutomaton(stdout, dfa, "d") catch unreachable;
    try self.appendAutomaton(&nerode, blk);
}

fn appendAutomaton(
    self: *PassManager,
    automaton: *ra.Automaton,
    blk: *ir.Block,
) PassError!void {
    for (automaton.blocks.items) |block| {
        const instr = block.insts.items[0];
        if (instr.tag != .RET) continue;

        const action = &self.ir.actions.items[instr.data.action];
        action.nonterms.clearAndFree();
        for (action.args.items) |*arg| {
            assert(arg.ret.isNone());
            arg.deinit(self.ir.allocator);
        }
        action.args.clearAndFree();
        action.base = block;
    }

    automaton.replaceBlock(automaton.start, blk);
    blk.deinitContent(self.ir.allocator);

    for (automaton.blocks.items) |block| {
        block.base = blk.base;

        if (block != automaton.start) {
            block.id = self.ir.blocks.items.len;
            try self.ir.blocks.append(block);
            continue;
        }

        block.id = blk.id;
        blk.* = block.*;
    }

    self.ir.allocator.destroy(automaton.start);
    automaton.blocks.deinit();
}

fn blockPass(
    self: *PassManager,
    comptime pass: fn (*PassManager, *ir.Block) PassError!void,
) !void {
    var i: usize = 0;
    while (i < self.ir.blocks.items.len) : (i += 1) {
        try pass(self, self.ir.blocks.items[i]);
    }
}
