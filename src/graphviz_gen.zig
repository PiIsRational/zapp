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
const pir = @import("peg_ir.zig");
const su = @import("string_utils.zig");
const lir_mod = @import("lir.zig");
const lir = lir_mod.ir;
const Automaton = lir_mod.Automaton;

pub fn generate(w: Writer, p_ir: pir.PegIr) !void {
    std.Progress.lockStdErr();
    defer std.Progress.unlockStdErr();

    try w.print("digraph {s} {{\n", .{p_ir.name});
    for (p_ir.grammar.defs.items) |def| {
        try genDef(w, def, p_ir);
    }
    try w.print("}}\n", .{});
}

fn genDef(w: Writer, def: pir.Definition, p_ir: pir.PegIr) !void {
    for (def.sequences.items) |seq| for (seq.operateds.items) |op| switch (op.value) {
        .ID => |id| {
            try genNode(def.id, w, p_ir);
            try w.print(" -> ", .{});
            try genNode(id, w, p_ir);
            try w.print(";\n", .{});
        },
        else => {},
    };
}

fn genNode(id: usize, w: Writer, p_ir: pir.PegIr) !void {
    const def = &p_ir.grammar.defs.items[id];
    if (def.generated()) {
        try w.print("node_{d}", .{id});
    } else {
        try w.print("{s}", .{def.identifier});
    }
}

pub fn genAutomaton(
    w: Writer,
    dfa: Automaton,
    name: []const u8,
) !void {
    std.Progress.lockStdErr();
    defer std.Progress.unlockStdErr();

    try w.print("digraph {s} {{ \n", .{name});
    try w.print("    node [shape = doublecircle]; ", .{});
    try getTerminals(w, dfa);
    try w.print(";\n    node [shape = circle];\n", .{});
    try w.print("    -1 [shape = point]\n", .{});
    try w.print("    -1 -> {d}\n", .{dfa.start.id});
    for (dfa.blocks.items) |block| {
        try genAutomatonNodeEps(w, block);
        try genAutomatonNode(w, block);
    }
    try w.print("}}\n", .{});
}

fn getTerminals(w: Writer, dfa: Automaton) !void {
    for (dfa.blocks.items) |block| if (block.insts.items.len != 0 and
        block.insts.items[0].tag == .RET)
    {
        try w.print(" {d}", .{block.id});
    };
}

fn genAutomatonNodeEps(w: Writer, block: *lir.Block) !void {
    if (!block.meta.is_target) return;
    var curr = block;
    while (curr.fail) |fail| : (curr = fail) {
        if (fail.fail == null) break;
        try w.print("    {d} -> {d} [label = \"ε\"];\n", .{ block.id, fail.id });
    }
}

fn genAutomatonNode(w: Writer, block: *lir.Block) !void {
    if (block.insts.items.len != 1) return;

    const instr = block.insts.items[0];
    switch (instr.tag) {
        .MATCH => for (instr.data.match.items) |prong| {
            try genAutomatonEdge(w, block, prong);
        },
        .JMP => try w.print(
            "    {d} -> {d} [label = \"ε\"];\n",
            .{ block.id, instr.data.jmp.id },
        ),
        else => {},
    }
}

fn genAutomatonEdge(w: Writer, start: *lir.Block, prong: lir.MatchProng) !void {
    try w.print("    {d} -> {d} [label = \"", .{ start.id, prong.dest.id });
    var len: usize = 0;
    if (!prong.consuming) try w.print("(ε) ", .{});
    for (prong.labels.items, 0..) |range, i| {
        try genRange(w, range);
        len += if (range.isChar()) 1 else 3;
        if (i + 1 == prong.labels.items.len) continue;

        if (len >= 7) {
            try w.print(",\\n", .{});
            len = 0;
        } else {
            try w.print(", ", .{});
            len += 1;
        }
    }
    try w.print("\"];\n", .{});
}

fn genRange(w: Writer, range: lir.Range) !void {
    try su.writeEscapeChar(w, range.from);

    if (range.isChar()) return;

    try w.print(" .. ", .{});
    try su.writeEscapeChar(w, range.to);
}
