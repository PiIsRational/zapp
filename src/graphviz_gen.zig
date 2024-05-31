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
const ir = @import("peg_ir.zig");
const PegIr = ir.PegIr;
const ReturnType = ir.ReturnType;
const ZigHeader = ir.ZigHeader;
const PegGrammar = ir.PegGrammar;
const Definition = ir.Definition;
const Sequence = ir.Sequence;
const Action = ir.Action;
const ActionVar = ir.ActionVar;
const Operated = ir.Operated;
const PrefixOp = ir.PrefixOp;
const PostfixOp = ir.PostfixOp;
const Primary = ir.Primary;
const Class = ir.Class;
const Range = ir.Range;

pub fn generate(w: Writer, p_ir: PegIr) !void {
    try w.print("digraph {s} {{\n", .{p_ir.name});
    for (p_ir.grammar.defs.items) |def| {
        try genDef(w, def, p_ir);
    }
    try w.print("}}\n", .{});
}

pub fn genDef(w: Writer, def: Definition, p_ir: PegIr) !void {
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

pub fn genNode(id: usize, w: Writer, p_ir: PegIr) !void {
    const def = &p_ir.grammar.defs.items[id];
    if (def.generated()) {
        try w.print("node_{d}", .{id});
    } else {
        try w.print("{s}", .{def.identifier});
    }
}
