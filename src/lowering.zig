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
const pir = @import("peg_ir.zig");
const lir = @import("low_ir.zig");

pub fn lower(allocator: Allocator, p_ir: pir.PegIr) !lir.LowIr {
    var def_names = std.ArrayList([]const u8).init(allocator);
    for (p_ir.defs.items) |def| {
        if (def.identifier.len == 0) break;
        try def_names.append(def.identifier);
    }

    var low_ir: lir.LowIr = .{
        .allocator = allocator,
        .file_name = p_ir.file_name,
        .parser_name = p_ir.name,
        .top_header = p_ir.top_header,
        .field_header = p_ir.field_header,
        .rule_names = try def_names.toOwnedSlice(),
        .top_level_comment = try allocator
            .dupe([]const u8, p_ir.top_level_comment),
        .actions = std.ArrayList(lir.Action).init(allocator),
        .blocks = std.ArrayList(*lir.Block).init(allocator),
        .place_to_block = std.AutoHashMap(lir.Label, *lir.Block)
            .init(allocator),
        .is_acceptor = p_ir.is_acceptor,
    };

    // init the blocks
    try initSeqBlocks(p_ir, &low_ir);

    // add the definitions
    for (p_ir.defs.items) |def| {
        try lowerDef(def, &low_ir);
    }

    return low_ir;
}

fn initSeqBlocks(p_ir: pir.PegIr, low_ir: *lir.LowIr) !void {
    const allocator = low_ir.allocator;
    for (p_ir.defs.items) |def| {
        const seqs = def.sequences.items;

        for (0..seqs.len) |i| {
            try initSeqBlock(seqs[seqs.len - 1 - i], low_ir, .{
                .def = def.id,
                .choice = i,
                .symbol = 0,
            });
        }

        try low_ir.appendBlock(try lir.Block.init(allocator), .{
            .def = def.id,
            .choice = seqs.len,
            .symbol = 0,
        });
    }
}

fn initSeqBlock(seq: pir.Sequence, low_ir: *lir.LowIr, place: lir.Label) !void {
    const allocator = low_ir.allocator;
    const ops = seq.operateds.items;

    // the start of the sequence is always needed
    try low_ir.appendBlock(try lir.Block.init(allocator), place);

    for (0..ops.len) |i| {
        const op = ops[ops.len - 1 - i];

        if (!cutSeq(op)) continue;

        // the break is after the current op
        try low_ir.appendBlock(try lir.Block.init(allocator), .{
            .def = place.def,
            .choice = place.choice,
            .symbol = i + 1,
        });
    }
}

/// checks if the operated needs the sequence to be cut after it
///
/// (this should only be needed for jumps or operations that get lowered to one)
fn cutSeq(op: pir.Operated) bool {
    return switch (op.value) {
        .IDENTIFIER,
        .SEQ,
        .EPSILON,
        => unreachable,
        .DOT, // goes to a match
        .CLASS, // goes to a match
        .ID, // contextful branching resumes after the jump
        .CUT, // no branching but the meta changes
        => true,
        .LITERAL, // no branching
        => false,
    };
}

fn lowerDef(def: pir.Definition, low_ir: *lir.LowIr) !void {
    const seqs = def.sequences.items;
    const fail_lbl: lir.Label = .{
        .def = def.id,
        .choice = seqs.len,
        .symbol = 0,
    };

    for (0..seqs.len) |i| {
        const seq = seqs[seqs.len - 1 - i];
        try lowerSeq(seq, low_ir, .{
            .def = def.id,
            .choice = i,
            .symbol = 0,
        }, fail_lbl, def);
    }

    var blk = low_ir.place_to_block.get(fail_lbl).?;
    blk.meta.nonterm_fail = true;
    try blk.insts.append(lir.Instr.initTag(
        if (def.id == 0) .EXIT_FAIL else .FAIL,
    ));
}

fn lowerSeq(
    seq: pir.Sequence,
    low_ir: *lir.LowIr,
    label: lir.Label,
    fail_lbl: lir.Label,
    def: pir.Definition,
) !void {
    const ops = seq.operateds.items;
    const last_branch = label.choice + 1 == fail_lbl.choice;
    var has_cut = false;

    var curr_blk = low_ir.place_to_block.get(label).?;
    curr_blk.fail = low_ir.place_to_block.get(
        if (last_branch) fail_lbl else .{
            .def = label.def,
            .choice = label.choice + 1,
            .symbol = 0,
        },
    );
    curr_blk.meta = .{
        .mid_recurse = def.mid_recurse,
        .right_recurse = def.right_recurse,
        .regular = def.regular,
        .finite = def.finite,
        .nonterm_fail = last_branch,
        .moves_actions = def.moves_actions,
    };

    for (0..ops.len) |i| {
        const op = ops[ops.len - 1 - i];
        has_cut = try lowerOp(
            op,
            curr_blk,
            low_ir,
            .{
                .def = label.def,
                .choice = label.choice,
                .symbol = i,
            },
        ) or has_cut;

        if (!cutSeq(op)) continue;

        // the break is after the current op
        curr_blk = low_ir.place_to_block.get(.{
            .def = label.def,
            .choice = label.choice,
            .symbol = i + 1,
        }).?;

        curr_blk.fail = low_ir.place_to_block.get(
            if (last_branch or has_cut) fail_lbl else .{
                .def = label.def,
                .choice = label.choice + 1,
                .symbol = 0,
            },
        );
        curr_blk.meta = .{
            .mid_recurse = def.mid_recurse,
            .right_recurse = def.right_recurse,
            .regular = def.regular,
            .finite = def.finite,
            .nonterm_fail = last_branch or has_cut,
            .moves_actions = def.moves_actions,
        };
    }

    var instr = lir.Instr.initTag(if (label.def == 0) .EXIT_PASS else .RET);

    instr.data = .{ .action = low_ir.actions.items.len };
    try curr_blk.insts.append(instr);

    const act = try lowerAction(
        &seq.action,
        curr_blk,
        low_ir,
        ops,
    );
    try low_ir.actions.append(act);
}

fn lowerAction(
    act: *const pir.Action,
    block: *lir.Block,
    low_ir: *lir.LowIr,
    ops: []const pir.Operated,
) !lir.Action {
    const allocator = low_ir.allocator;
    var args = std.ArrayList(lir.LabeledRet).init(allocator);
    defer args.deinit();
    var nonterms = std.ArrayList(lir.CallPattern).init(allocator);
    defer nonterms.deinit();

    var space: usize = 0;
    for (0..ops.len) |i| {
        const op = ops[ops.len - 1 - i];

        if (op.prefix_op != .NONE) continue;

        switch (op.value) {
            .ID => |id| {
                try nonterms.append(.{
                    .consumes = space,
                    .calls = low_ir.place_to_block.get(.{
                        .def = id,
                        .choice = 0,
                        .symbol = 0,
                    }).?,
                });
                space = 0;
            },
            .LITERAL => |lit| space += lit.len,
            .CLASS, .DOT => space += 1,
            .CUT => {},
            else => unreachable,
        }
    }

    var curr_arg: usize = 0;
    for (act.rets.items) |ret| {
        var mut = false;
        for (act.bases.items) |base| for (base.action_vars.items) |a_var| {
            if (a_var.arg_num) |num| {
                if (num != ret.var_name or !a_var.escape) continue;
                mut = true;
                break;
            }
        };

        try args.append(.{
            .ret = ret.ret_op.return_type,
            .place = low_ir.place_to_block.get(.{
                .def = ret.ret_op.value.ID,
                .choice = 0,
                .symbol = 0,
            }).?,
            .lbl = ret.var_name,
            .needs_mut = mut,
        });

        if (ret.ret_op.return_type.isNone()) continue;

        curr_arg += 1;
    }

    return try lir.Action.init(
        allocator,
        args,
        block,
        nonterms,
        space,
        act.bases,
    );
}

/// returns true iff the sequence has been cut
fn lowerOp(
    op: pir.Operated,
    block: *lir.Block,
    low_ir: *lir.LowIr,
    lbl: lir.Label,
) !bool {
    const allocator = low_ir.allocator;
    var instr: lir.Instr = .{
        .tag = undefined,
        .meta = .{
            .pos = op.prefix_op == .AND,
            .neg = op.prefix_op == .NOT,
        },
        .data = undefined,
    };

    switch (op.value) {
        .ID => |id| {
            instr.tag = .NONTERM;
            instr.data = .{ .ctx_jmp = .{
                .next = low_ir.place_to_block.get(.{
                    .def = id,
                    .choice = 0,
                    .symbol = 0,
                }).?,
                .returns = low_ir.place_to_block.get(.{
                    .def = lbl.def,
                    .choice = lbl.choice,
                    .symbol = lbl.symbol + 1,
                }).?,
            } };
        },
        .LITERAL => |lit| {
            instr.tag = .STRING;
            instr.data = .{
                .str = try allocator.dupe(u8, lit),
            };
        },
        .CLASS => |c| {
            instr.tag = .MATCH;
            var ranges = std.ArrayList(lir.Range).init(allocator);
            for (c.content.items) |range| {
                try ranges.append(.{
                    .from = range.from,
                    .to = if (range.to == 0) range.from else range.to,
                });
            }
            var prong: lir.MatchProng = .{
                .labels = ranges,
                .dest = low_ir.place_to_block.get(.{
                    .def = lbl.def,
                    .choice = lbl.choice,
                    .symbol = lbl.symbol + 1,
                }).?,
                .consuming = op.prefix_op == .NONE,
            };
            if (op.prefix_op == .NOT) {
                try prong.invert();
            }
            var prong_list = std.ArrayList(lir.MatchProng).init(allocator);
            try prong_list.append(prong);
            instr.data = .{
                .match = prong_list,
            };
        },
        .DOT => {
            instr.tag = .MATCH;
            var ranges = std.ArrayList(lir.Range).init(allocator);
            try ranges.append(.{
                .from = 1, // 0 is not allowed
                .to = 255,
            });
            var prong: lir.MatchProng = .{
                .labels = ranges,
                .dest = low_ir.place_to_block.get(.{
                    .def = lbl.def,
                    .choice = lbl.choice,
                    .symbol = lbl.symbol + 1,
                }).?,
                .consuming = op.prefix_op == .NONE,
            };
            if (op.prefix_op == .NOT) {
                try prong.invert();
            }
            var prong_list = std.ArrayList(lir.MatchProng).init(allocator);
            try prong_list.append(prong);
            instr.data = .{
                .match = prong_list,
            };
        },
        .CUT => {
            // add an implicit jump to the next block
            instr.tag = .JMP;
            instr.data = .{ .jmp = low_ir.place_to_block.get(.{
                .def = lbl.def,
                .choice = lbl.choice,
                .symbol = lbl.symbol + 1,
            }).? };

            try block.insts.append(instr);
            return true;
        },
        else => unreachable,
    }

    try block.insts.append(instr);
    return false;
}
