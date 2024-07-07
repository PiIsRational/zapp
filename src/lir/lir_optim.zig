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
    const stdout = std.io.getStdOut().writer();
    const self: PassManager = .{
        .ir = lir,
    };

    var emitter = LookaheadEmitter.init(self.ir.allocator);
    defer emitter.deinit();

    const nfa = try emitter.emit(self.ir.blocks.items[0]);

    defer nfa.deinit(self.ir.allocator);

    var dfa_gen = DfaGen.init(self.ir.allocator);
    defer dfa_gen.deinit();
    const dfa = try dfa_gen.genDfa(nfa.start);
    defer dfa.deinit(self.ir.allocator);

    if (false) {
        try gvgen.genAutomaton(stdout, nfa, "nfa");
        try gvgen.genAutomaton(stdout, dfa, "dfa");
    }

    const nerode = try minimize(self.ir.allocator, dfa);
    defer nerode.deinit(self.ir.allocator);

    try gvgen.genAutomaton(stdout, dfa, "dfa");
    try gvgen.genAutomaton(stdout, nerode, "nerode");
}

fn generateDfa(self: *PassManager, block: *ir.Block) !void {
    assert(block.meta.is_terminal);
    assert(block.meta.is_target);

    _ = self;
}

/// equivalent to cloning a rule in pir
fn shallowRuleCopy(self: *PassManager, block: *ir.Block, base: usize) !*ir.Block {
    assert(block.meta.is_target);
    var map = std.AutoHashMap(usize, *ir.Block).init(self.ir.allocator);
    defer map.deinit();
    return try self.shallowRuleCopyRec(block, base, &map);
}

fn shallowRuleCopyRec(
    self: *PassManager,
    block: *ir.Block,
    base: usize,
    map: *std.AutoHashMap(usize, *ir.Block),
) !*ir.Block {
    const allocator = self.ir.allocator;
    // 3 steps
    // 1) copy the own block over or get it from the map
    if (map.get(block.id)) |clone| return clone;
    var new_block = try block.clone(allocator);
    try self.ir.appendDefBlock(new_block, base);
    try map.put(block.id, new_block);

    // 2) copy the fail block
    if (block.fail) |fail_block| {
        new_block.fail = try self.shallowRuleCopyRec(fail_block, base, map);
    }

    // 3) copy the jump to blocks
    const insts = new_block.insts.items;
    const last = &insts[insts.len - 1];
    switch (last.tag) {
        .JMP => {
            const jmp_block = last.data.jmp;
            last.data.jmp = try self.shallowRuleCopyRec(jmp_block, base, map);
        },
        .MATCH => {
            const prongs = last.data.match.items;
            for (prongs) |*prong| {
                prong.dest = try self.shallowRuleCopyRec(prong.dest, base, map);
            }
        },
        .NONTERM => {
            const jmp_block = last.data.ctx_jmp.next;
            last.data.ctx_jmp.next = try self.shallowRuleCopyRec(jmp_block, base, map);
        },
        .FAIL,
        .RET,
        .EXIT_PASS,
        .EXIT_FAIL,
        => {},
        else => unreachable,
    }

    return new_block;
}

fn blockPass(
    self: *PassManager,
    comptime pass: fn (*PassManager, *ir.Block) PassError!void,
) !void {
    const blocks = &self.ir.blocks.items;
    var i: usize = 0;
    while (i < blocks.len) : (i += 1) {
        try pass(self, blocks[i]);
    }
}
