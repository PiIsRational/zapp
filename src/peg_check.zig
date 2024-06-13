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
const ir = @import("peg_ir.zig");
const Error = @import("errors.zig");

const PegPassManager = @This();

idents: std.StringHashMap(usize),
ir: *ir.PegIr,
pass: bool,
relations: []bool,
reachable: []bool,
main_progress: std.Progress.Node,

pub fn optimize(
    self: *PegPassManager,
    pIr: *ir.PegIr,
    p_node: *std.Progress.Node,
    inlining: bool,
) !bool {
    self.main_progress = p_node.start("optimize pir", 0);
    defer self.main_progress.end();
    const allocator = pIr.allocator;
    self.ir = pIr;
    self.pass = true;
    self.idents = std.StringHashMap(usize).init(allocator);
    defer self.idents.deinit();

    try self.defPass(false, doubleDefCheck, "check no doubled defs");
    try self.defPass(false, checkNoActionReturn, "check no return without action");
    if (!self.pass) return false;
    try self.opPass(checkIdentifier, "no use of undefined identifiers");
    if (!self.pass) return false;
    try self.seqPass(desugarIdentifierSequence, "desugar identifiers");
    try self.opAppendDefPass(desugarSequences, "desugar sequences");
    try self.opPass(checkEpsWithOp, "check no weird operations on epsilon");
    if (!self.pass) return false;
    try self.defPass(true, typingPass, "add typing");
    try self.seqPass(appendImplicitActions, "append implicit actions");
    try self.seqPass(checkActionVars, "check action validity");
    try self.opPass(checkClass, "verify classes are correct");
    try self.seqAppendDefPass(desugarPlusSequence, "desugar plus sequences");
    try self.seqAppendDefPass(desugarStarSequence, "desugar star sequences");
    try self.opAppendDefPass(desugarQuestionOp, "desugar questions");
    try self.seqPass(removeEps, "eliminate epsilon");

    const rule_count = self.ir.defs.items.len;
    for (0..rule_count) |_| try self.defPass(
        false,
        setAcceptsEps,
        "check for epsilon acceptance",
    );

    self.relations = try allocator.alloc(
        bool,
        rule_count * rule_count,
    );
    @memset(self.relations, false);

    // TODO: make this faster
    for (0..rule_count) |_| try self.defPass(false, buildRelations, "analyze reachability");
    allocator.free(self.relations);

    self.reachable = try allocator.alloc(bool, rule_count);
    defer allocator.free(self.reachable);
    @memset(self.reachable, false);
    try self.checkAccess();
    try self.defPass(false, sameAsRoot, "check reachable from start");
    try self.deleteUnreachableDefs();
    if (!self.pass) return false;

    self.ir.is_acceptor = true;
    try self.seqPass(checkIsAcceptor, "check for no actions");
    try self.findRecursions();
    try self.seqPass(setActionRets, "add rets to actions");

    try self.updateReachable();
    for (0..self.ir.defs.items.len) |_| try self.defPass(
        false,
        findEmptyRules,
        "try to find unmatchable rules",
    );
    try self.defPass(false, errorOnEmptyRules, "maybe error out on empty rules");
    if (!self.pass) return false;
    try self.defPass(false, addActionReturnType, "add return types to actions");

    // single sequence inlining
    if (inlining) {
        try self.defPass(false, inlineOneSeqDefs, "inline single sequence defs");
        try self.checkAccess();
        try self.deleteUnreachableDefs();
    }

    try self.seqPass(checkDoubledCut, "check for doubled cut ops");
    try self.defPass(false, checkLastSeqCut, "check for cuts in ending sequences");
    if (!self.pass) return false;

    try self.defPass(false, setInitialUseActions, "init moves actions");
    for (0..self.ir.defs.items.len) |_| try self.defPass(
        false,
        updateUseActions,
        "use action step",
    );

    return self.pass;
}

/// the error message writer for `findEmptyRules`
fn errorOnEmptyRules(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    if (self.reachable[def.id]) return;

    self.pass = false;
    try Error.print(
        "empty acceptance",
        self.ir.file_name,
        self.ir.chars,
        &.{.{
            .place = def.identifier,
            .msg = "cannot match anything",
            .t = .ERROR,
        }},
    );
}

/// sets use action for the first time using actions
/// every time after that it should not look at actions anymore
///
/// needs:
///     * all actions should be finalized
/// change bounds:
///     * sets `moves_actions` for rules that contain actions
fn setInitialUseActions(_: *PegPassManager, def: *ir.Definition) anyerror!void {
    for (def.sequences.items) |seq| {
        if (seq.action.isEmpty()) continue;

        def.moves_actions = true;
        return;
    }
}

/// updates use actions
///
/// needs:
///     * `setInitUseActions` should be called just before
/// change bounds:
///     * executes one step of the use action alg
fn updateUseActions(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    if (def.moves_actions) return;

    for (def.sequences.items) |seq| for (seq.operateds.items) |op| {
        switch (op.value) {
            .ID => |id| {
                const child_def = self.getDefinition(id);
                if (!child_def.moves_actions) continue;

                def.moves_actions = true;
                return;
            },
            else => {},
        }
    };
}

/// adds the retutrn type to the actions
///
/// needs:
///     * every action should be finalized
///     * no inlining by now
/// change bounds:
///     * adds the correct return type to actions
fn addActionReturnType(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    const allocator = self.ir.allocator;
    for (def.sequences.items) |seq| {
        const action = seq.action;
        if (action.isEmpty()) continue;
        const bases = action.bases.items;
        assert(bases.len == 1);
        const base = &bases[0];
        assert(base.return_type.isImplicit());
        base.return_type = try def.return_type.clone(allocator);
    }
}

/// this pass tries to catch most empty rules
/// it is not perfect as finding empty rules is undeciiable in PEG
/// the way it is handeled is through treating lookahead (&, !) the
/// same as normal nonterminals
///
/// (the pass uses the reachable slice which should be reset before)
///
/// needs:
///     * everything should be canonicalized
/// asserts:
///     * mostly no empty rules
///      (no empty rules through forced recursion)
fn findEmptyRules(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    for (def.sequences.items) |seq| {
        var good = true;
        for (seq.operateds.items) |op| {
            switch (op.value) {
                .ID => |id| if (!self.reachable[id]) {
                    good = false;
                    break;
                },
                else => {},
            }
        }

        if (good) {
            self.reachable[def.id] = true;
            break;
        }
    }
}

fn updateReachable(self: *PegPassManager) !void {
    const allocator = self.ir.allocator;
    const rule_count = self.ir.defs.items.len;
    self.reachable = try allocator.realloc(self.reachable, rule_count);
    @memset(self.reachable, false);
}

/// a struct used for the backtracking stack of a dfs
fn Orientation(extra: type) type {
    return struct {
        rule_id: usize,
        seq_id: usize,
        op_id: usize,
        other: extra,

        pub fn index(orient: @This(), pass: *PegPassManager) ?*ir.Operated {
            const defs = pass.ir.defs.items;
            const seqs = defs[orient.rule_id].sequences.items;
            if (orient.seq_id >= seqs.len) return null;
            const ops = seqs[orient.seq_id].operateds.items;
            if (orient.op_id >= ops.len) return null;
            return &ops[orient.op_id];
        }
    };
}

/// an enum used to "paint" rules
const NodeState = enum {
    NOT_FOUND,
    FOUND,
    VISITED,
};

/// set the recursions for the definition
///
/// needs:
///     * left recursions are eliminated
fn findRecursions(self: *PegPassManager) !void {
    const allocator = self.ir.allocator;

    const Orient = Orientation(struct {
        left: bool,
        right: bool,
        next_id: ?usize,
    });
    var backtracking = std.ArrayList(Orient).init(allocator);
    defer backtracking.deinit();

    const rules = self.ir.defs.items;
    // no rule should be set recursive here
    for (rules) |rule| assert(!rule.right_recurse);
    for (rules) |rule| assert(!rule.mid_recurse);
    for (rules) |rule| assert(rule.finite);
    for (rules) |rule| assert(rule.regular);

    var flags = try allocator.alloc(NodeState, rules.len);
    defer allocator.free(flags);
    @memset(flags, .NOT_FOUND);

    try backtracking.append(.{
        .rule_id = 0,
        .seq_id = 0,
        .op_id = 0,
        .other = .{
            .left = false,
            .right = false,
            .next_id = null,
        },
    });
    flags[0] = .FOUND;

    while (backtracking.popOrNull()) |o| {
        var orient = o;
        if (orient.other.next_id) |id| {
            var this_rule = self.getDefinition(orient.rule_id);
            const next_rule = self.getDefinition(id);

            if (!next_rule.regular) this_rule.regular = false;
            if (!next_rule.finite) this_rule.finite = false;
        }

        if (orient.index(self)) |op| {
            orient.other.left = orient.op_id != 0;
            orient.op_id += 1;
            orient.other.right = orient.index(self) != null;
            orient.other.next_id = switch (op.value) {
                .ID => |id| id,
                else => null,
            };

            try backtracking.append(orient);

            switch (op.value) {
                .ID => |id| if (flags[id] == .NOT_FOUND) {
                    try backtracking.append(.{
                        .rule_id = id,
                        .seq_id = 0,
                        .op_id = 0,
                        .other = .{
                            .next_id = null,
                            .left = false,
                            .right = false,
                        },
                    });
                    flags[id] = .FOUND;
                    continue;
                } else if (flags[id] == .FOUND) {
                    var recursion: []Orient = undefined;
                    var set_recursion = false;
                    for (backtracking.items, 0..) |frame, i| {
                        if (id != frame.rule_id) continue;
                        recursion = backtracking.items[i..];
                        set_recursion = true;
                    }

                    if (!set_recursion) {
                        const next_rule = self.getDefinition(id);
                        const rule = self.getDefinition(orient.rule_id);
                        std.debug.print("{s}\n", .{rule});
                        std.debug.print("{s}\n", .{next_rule});
                        unreachable;
                    }

                    var found_left = false;
                    var found_right = false;
                    for (recursion) |frame| {
                        if (frame.other.right) found_right = true;
                        if (frame.other.left) found_left = true;
                    }

                    assert(found_right); // otherwise would point to left recursion

                    for (recursion) |frame| {
                        var rule = self.getDefinition(frame.rule_id);
                        rule.right_recurse = !found_left and !rule.mid_recurse;
                        rule.mid_recurse = found_left;
                        rule.finite = false;
                        rule.regular = rule.regular and rule.right_recurse;
                    }
                },
                else => {},
            }
            continue;
        }

        orient.seq_id += 1;
        orient.op_id = 0;

        if (orient.index(self) != null) {
            try backtracking.append(orient);
            continue;
        }

        flags[orient.rule_id] = .VISITED;
    }
}

/// sets the rets for the actions in the ir
///
/// needs:
///     * the ops should be typed
///     * the actions should be checked
///     * all canonicalizations should be passed
/// change bounds:
///     * the action rets are set
fn setActionRets(_: *PegPassManager, seq: *ir.Sequence) !void {
    try seq.action.setRets(seq.*);
}

/// verifies that there are no cut operators in the last sequence of a definition
///
/// needs:
///     * every operation should be expanded
///     * inlining should have taken place
/// asserts:
///     * no misplaced cut operators in the grammar
///      (with the `checkDoubledCut` pass)
fn checkLastSeqCut(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    const last_seq = def.sequences.items[0];
    var fail: bool = true;
    for (last_seq.operateds.items) |op| {
        fail = switch (op.value) {
            .CUT => true,
            else => false,
        };

        if (!fail) continue;

        self.pass = false;
        try Error.print(
            "misplaced cut operator",
            self.ir.file_name,
            self.ir.chars,
            &.{.{
                .place = op.string,
                .msg = "cut in the last sequence of a definition",
                .t = .ERROR,
            }},
        );

        return;
    }
}

/// verifies that there are no doubled cut operators in the ir
/// (or cut operators that would not make sense e.x. at the end of the sequence)
///
/// needs:
///     * every operation should be expanded
///     * inlining should have taken place
/// asserts:
///     * no misplaced cut operators in the grammar
///      (with the `checkLastSeqCut` pass)
fn checkDoubledCut(self: *PegPassManager, seq: *ir.Sequence) anyerror!void {
    var fail = false;
    var found_cut = false;
    var other_str: []const u8 = undefined;
    const ops = seq.operateds.items;
    for (ops, 0..) |op, i| {
        assert(op.postfix_op == .NONE);
        switch (op.value) {
            .CUT => {
                fail = found_cut or i == 0 or i == ops.len - 1;
                if (!found_cut) other_str = op.string;
                found_cut = !found_cut;
            },
            else => {},
        }

        if (!fail) continue;

        if (!found_cut) {
            try Error.print(
                "misplaced cut operator",
                self.ir.file_name,
                self.ir.chars,
                &.{ .{
                    .place = op.string,
                    .msg = "doubled cut",
                    .t = .ERROR,
                }, .{
                    .place = other_str,
                    .msg = "other cut here",
                    .t = .INFO,
                } },
            );
        } else {
            try Error.print(
                "misplaced cut operator",
                self.ir.file_name,
                self.ir.chars,
                &.{.{
                    .place = op.string,
                    .msg = if (i == 0)
                        "cut at the end of a sequence"
                    else
                        "cut at the start of a sequence",
                    .t = .ERROR,
                }},
            );
        }

        self.pass = false;
        return;
    }
}

/// verifies if the ir is just an acceptor (has no actions)
///
/// needs:
///     * everythin should be expanded
///     * `self.ir.is_acceptor` should be set to `true`
/// change bounds:
///     * `self.ir.is_acceptor` is set to `false` if an action is encountered
fn checkIsAcceptor(self: *PegPassManager, seq: *ir.Sequence) anyerror!void {
    if (!seq.action.isEmpty()) {
        self.ir.is_acceptor = false;
    }
}

fn deleteUnreachableDefs(self: *PegPassManager) !void {
    const def_cnt = self.reachable.len;
    for (0..def_cnt) |i| {
        const id = def_cnt - 1 - i;
        if (self.reachable[id]) continue;
        try self.deleteDef(id);
    }
}

fn deleteDef(self: *PegPassManager, def: usize) !void {
    const OpUpdate = struct {
        var def_num: usize = 0;

        pub fn update(pass: *PegPassManager, op: *ir.Operated) anyerror!void {
            _ = pass;
            switch (op.value) {
                .ID => |*id| if (id.* > def_num) {
                    id.* -= 1;
                },
                .SEQ,
                .IDENTIFIER,
                .EPSILON,
                => unreachable,
                else => {},
            }
        }
    };

    const DefUpdate = struct {
        var def_num: usize = 0;

        pub fn update(pass: *PegPassManager, defn: *ir.Definition) anyerror!void {
            _ = pass;
            if (defn.id > def_num) defn.id -= 1;
        }
    };

    OpUpdate.def_num = def;
    DefUpdate.def_num = def;
    var defs = &self.ir.defs;
    const allocator = self.ir.allocator;
    defs.items[def].deinit(allocator);
    _ = defs.orderedRemove(def);
    try self.opPass(OpUpdate.update, "update ops for delete");
    try self.defPass(false, DefUpdate.update, "delete ops");
}

fn sameAsRoot(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    if (self.reachable[def.id]) return;
    if (def.generated()) return;
    try Error.print(
        "unreachable definition",
        self.ir.file_name,
        self.ir.chars,
        &.{.{
            .place = def.identifier,
            .msg = "not reachable from start",
            .t = .WARNING,
        }},
    );
}

fn checkAccess(self: *PegPassManager) !void {
    try self.updateReachable();
    // doing a dfs
    const allocator = self.ir.allocator;
    var search_stack = std.ArrayList(usize).init(allocator);
    defer search_stack.deinit();
    try search_stack.append(0);
    self.reachable[0] = true;

    while (search_stack.popOrNull()) |id| {
        const def = self.getDefinition(id);
        const seqs = def.sequences.items;

        for (seqs) |seq| for (seq.operateds.items) |op| switch (op.value) {
            .ID => |next_id| if (!self.reachable[next_id]) {
                self.reachable[next_id] = true;
                try search_stack.append(next_id);
            },
            .IDENTIFIER,
            .SEQ,
            .EPSILON,
            => unreachable,
            else => {},
        };
    }
}

/// checks that there are no doubled definitions
///
/// needs: none
/// asserts:
///     * no double def
/// change bounds:
///     * the literals are set
fn doubleDefCheck(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    const result = try self.idents.fetchPut(
        def.identifier,
        def.id,
    );

    if (result) |ident| {
        try Error.print(
            "cannot define nonterminals multiple times",
            self.ir.file_name,
            self.ir.chars,
            &.{ .{
                .place = def.identifier,
                .msg = "tried to redefine here",
                .t = .ERROR,
            }, .{
                .place = ident.key,
                .msg = "defined here",
                .t = .INFO,
            } },
        );

        self.pass = false;
    }
}

/// verify that for definitions that return values, that there are no empty actions
///
/// needs: none
/// asserts:
///     * no empty expressions
fn checkNoActionReturn(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    for (def.sequences.items) |seq| {
        if (!def.return_type.isNone() and seq.action.isEmpty()) {
            try Error.print(
                "missing action for sequence",
                self.ir.file_name,
                self.ir.chars,
                &.{ .{
                    .place = seq.string[0..1],
                    .msg = "action missing at the end of this sequence",
                    .t = .ERROR,
                }, .{
                    .place = def.return_type.getString().?,
                    .msg = "return type defined here",
                    .t = .INFO,
                } },
            );

            self.pass = false;
        }
    }
}

/// checks that the action of the sequence does use only existing variables
///
/// needs:
///     * the return values of the actions should be defined
///     * there should be no inlined actions here (bases.len == 1)
/// asserts:
///     * actions use only available vars
fn checkActionVars(self: *PegPassManager, seq: *ir.Sequence) anyerror!void {
    var arg_count: usize = 0;
    const action = seq.action;
    const ops = seq.operateds.items;
    const allocator = self.ir.allocator;
    const action_vars = if (action.isEmpty())
        &.{}
    else
        action.bases.items[0].action_vars.items;
    outer: for (0..ops.len) |i| {
        const op = ops[ops.len - 1 - i];
        assert(!op.return_type.isImplicit());

        arg_count += 1;
        if (op.return_type.isNone())
            continue;

        for (action_vars) |action_var| {
            if (action_var.arg_num) |num| {
                if (num + 1 == arg_count) continue :outer;
            }
        }

        const msg = try std.fmt.allocPrint(
            allocator,
            "returns \"{s}\" which could leak if ignored",
            .{op.return_type},
        );
        defer allocator.free(msg);
        try Error.print(
            "unused action var",
            self.ir.file_name,
            self.ir.chars,
            &.{.{
                .place = op.string,
                .msg = msg,
                .t = .ERROR,
            }},
        );
        self.pass = false;
    }

    for (action_vars) |action_var| {
        if (action_var.arg_num) |num| {
            const num_op = ops[ops.len - 1 - num];
            if (!num_op.return_type.isNone()) continue;

            self.pass = false;
            try Error.print(
                "disallowed action var",
                self.ir.file_name,
                self.ir.chars,
                &.{ .{
                    .place = action_var.var_decl,
                    .msg = "action var references nonreturning operated",
                    .t = .ERROR,
                }, switch (num_op.value) {
                    .ID => |id| .{
                        .place = self.ir.defs.items[id].identifier,
                        .msg = "nonterminal defined here",
                        .t = .INFO,
                    },
                    .SEQ,
                    .IDENTIFIER,
                    => unreachable,
                    else => .{
                        .place = num_op.string,
                        .msg = "this is not a nonterminal and thus cannot return!",
                        .t = .INFO,
                    },
                } },
            );
        }
    }
}

/// check that all ranges in a class are correct
///
/// needs: none
/// asserts:
///     * all classes are correct
fn checkClass(self: *PegPassManager, op: *ir.Operated) anyerror!void {
    const cls = switch (op.value) {
        .CLASS => |cl| cl,
        else => return,
    };

    for (cls.content.items) |range| {
        try self.checkRange(range);
    }
}

/// check that ranges are correct
///
/// needs: none
/// asserts:
///     * ranges are correct
fn checkRange(self: *PegPassManager, r: ir.Range) !void {
    if (r.isChar() or r.from < r.to) {
        return;
    }

    try Error.print(
        "illegal range",
        self.ir.file_name,
        self.ir.chars,
        &.{ .{
            .place = r.backing,
            .msg = "too large starting character",
            .t = .ERROR,
        }, .{
            .place = r.backing,
            .msg = if (r.from == r.to)
                "try removing the range operator"
            else
                "try inverting the chars",
            .t = .INFO,
        } },
    );

    self.pass = false;
}

/// checks that used identifiers are defined
///
/// needs:
///     * the identifiers hash map should be populated
fn checkIdentifier(self: *PegPassManager, op: *ir.Operated) anyerror!void {
    const ident = switch (op.value) {
        .IDENTIFIER => |id| id,
        .ID => unreachable,
        else => return,
    };

    if (self.idents.contains(ident))
        return;

    try Error.print(
        "trying to access inexistent nonterminal",
        self.ir.file_name,
        self.ir.chars,
        &.{.{ .place = ident, .msg = "", .t = .ERROR }},
    );

    self.pass = false;
}

/// check that there is no left recusion
///
/// needs:
///     * accpepts_eps needs to be set correctly
/// asserts:
///     * no possible left recursions in the grammar
fn buildRelations(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    const seqs = def.sequences.items;
    for (seqs) |seq| {
        const ops = seq.operateds.items;
        var i = ops.len;
        var accepts_eps = true;

        while (i > 0 and accepts_eps) {
            i -= 1;
            assert(ops[i].postfix_op == .NONE);

            switch (ops[i].value) {
                .ID => |other_id| {
                    try self.addRelation(
                        def.id,
                        other_id,
                        ops[i].string,
                    );

                    accepts_eps = self.getDefinition(other_id)
                        .accepts_eps;
                },
                .IDENTIFIER,
                .SEQ,
                .EPSILON,
                => unreachable,
                else => accepts_eps = false,
            }
        }
    }
}

fn addRelation(
    self: *PegPassManager,
    from: usize,
    to: usize,
    to_rule: []const u8,
) !void {
    const defs = self.ir.defs.items;

    var was_set = from == to and !self.relations[from * defs.len + to];
    self.relations[from * defs.len + to] = true;

    for (self.relations[to * defs.len .. (to + 1) * defs.len], 0..) |val, i| {
        if (val and i == from and !self.relations[from * defs.len + i]) {
            was_set = true;
        }

        self.relations[from * defs.len + i] = val or
            self.relations[from * defs.len + i];
    }

    if (!was_set)
        return;

    self.pass = false;

    try Error.print(
        "found left recursion",
        self.ir.file_name,
        self.ir.chars,
        if (defs[from].generated())
            &.{.{
                .place = to_rule,
                .msg = "cannot be matched",
                .t = .ERROR,
            }}
        else
            &.{ .{
                .place = to_rule,
                .msg = "can be produced to its own definition",
                .t = .ERROR,
            }, .{
                .place = defs[from].identifier,
                .msg = "definition declared here",
                .t = .INFO,
            } },
    );
}

/// converts all identifier operateds to their number id
///
/// needs:
///     * the identifiers hash map should be populated
/// change bounds:
///     * no operateds have the `IDENTIFIER` field populated
fn desugarIdentifierSequence(self: *PegPassManager, seq: *ir.Sequence) anyerror!void {
    for (seq.operateds.items) |*op| {
        op.value = switch (op.value) {
            .IDENTIFIER => |ident| .{ .ID = self.idents.get(ident).? },
            .SEQ => |*sequence| blk: {
                try self.desugarIdentifierSequence(sequence);
                break :blk op.value;
            },
            else => op.value,
        };
    }
}

/// converts all operateds that have a star to the corresponding definition
///
/// needs:
///     * no operateds with the plus operator should exist
/// change bounds:
///     * no operateds with the star operator will exist
fn desugarStarSequence(
    self: *PegPassManager,
    def: *const ir.Definition,
    seq: *ir.Sequence,
) anyerror!?ir.Definition {
    var i: usize = 0;
    const allocator = self.ir.allocator;
    while (i < seq.operateds.items.len) : (i += 1) {
        const op = &seq.operateds.items[i];
        if (op.postfix_op != .STAR) {
            continue;
        }

        var sequences = std.ArrayList(ir.Sequence).init(allocator);
        var body_ops = std.ArrayList(ir.Operated).init(allocator);
        const end_ops = std.ArrayList(ir.Operated).init(allocator);
        const new_id = self.ir.defs.items.len;
        try body_ops.append(ir.Operated.initId(
            new_id,
            try op.return_type.clone(allocator),
            .NONE,
            .NONE,
            op.string,
        ));
        var new_op = try op.clone(allocator);
        new_op.postfix_op = .NONE;
        new_op.prefix_op = .NONE;
        try self.setReturnType(&new_op);
        try body_ops.append(new_op);
        const ActionList = std.ArrayList(ir.ActionVar);
        try sequences.append(.{
            .operateds = end_ops,
            .action = if (op.return_type.isExistant()) .{
                .rets = std.ArrayList(ir.ActionReturn).init(allocator),
                .bases = blk: {
                    var bases = std.ArrayList(ir.ActionBase).init(allocator);
                    try bases.append(.{
                        .data = try std.fmt.allocPrint(
                            allocator,
                            "{s}.init(self.allocator)",
                            .{op.return_type},
                        ),
                        .owned = true,
                        .action_vars = ActionList.init(allocator),
                        .return_type = ir.ReturnType.impl(),
                        .id = 0,
                        .mut = false,
                    });
                    break :blk bases;
                },
                .implicit = false,
                .use_match = false,
            } else ir.Action.empty(allocator),
            .string = "",
        });
        try sequences.append(.{
            .operateds = body_ops,
            .action = if (!op.return_type.isNone())
                try ir.Action.popAddList(allocator, 1, 0)
            else
                ir.Action.empty(allocator),
            .string = "",
        });
        op.value.deinit(allocator);
        op.value = .{ .ID = new_id };
        op.postfix_op = .NONE;

        const new_def: ir.Definition = .{
            .identifier = def.identifier,
            .return_type = try op.return_type.clone(allocator),
            .sequences = sequences,
            .id = self.ir.defs.items.len,
            .accepts_eps = false,
            .mid_recurse = false,
            .right_recurse = false,
            .regular = true,
            .finite = true,
            .moves_actions = false,
            .is_terminal = def.is_terminal,
        };

        return new_def;
    }

    return null;
}

/// this pass checks that there are no epsilons with operators
///
/// needs:
///     * sequences should be canonicalized
/// asserts:
///     * no weird epsilons
fn checkEpsWithOp(self: *PegPassManager, op: *ir.Operated) anyerror!void {
    switch (op.value) {
        .EPSILON => {},
        // an empty literal is essentially an epsilon
        .LITERAL => |lit| if (lit.len > 0) return,
        else => return,
    }

    if (op.prefix_op == .NONE and op.postfix_op == .NONE) return;
    self.pass = false;

    try Error.print("operations on epsilon", self.ir.file_name, self.ir.chars, &.{.{
        .place = op.string,
        .msg = if (op.prefix_op == .NOT and op.postfix_op == .NONE)
            "this alway fails"
        else
            // TODO: if helps are implemented add a help here
            "effectively the same as epsilon",
        .t = .ERROR,
    }});
}

/// this pass sets the type for all operateds and definitions with an implicit
/// return type.
///
/// for the pass to work it needs to go backwards through the types.
///
/// needs:
///     * no operated sequences
fn typingPass(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    for (def.sequences.items) |*seq| {
        for (seq.operateds.items) |*op| {
            assert(op.return_type.isImplicit() or op.return_type.isNone());
            try self.setReturnType(op);
        }
    }

    if (!def.return_type.isImplicit()) return;
    for (def.sequences.items) |*seq| {
        if (!seq.action.isEmpty()) {
            seq.action.implicit = true;
        }
    }

    // the inferred type for the definition (if needed)
    // is the tupelized type of the first branch
    const allocator = self.ir.allocator;
    var returns = std.ArrayList(ir.ReturnType).init(allocator);
    defer returns.deinit();

    const first_sequence = def.sequences.items[0];
    for (first_sequence.operateds.items) |op| {
        assert(!op.return_type.isImplicit());
        try returns.append(op.return_type);
    }

    def.return_type = try ir.ReturnType.tupelize(allocator, returns.items);
}

/// resolve implicit actions
///
/// needs:
///     * all types should be set
/// change bounds:
///     * no implicit actions are left
fn appendImplicitActions(self: *PegPassManager, seq: *ir.Sequence) anyerror!void {
    if (!seq.action.implicit) return;
    const allocator = self.ir.allocator;
    var return_types = std.ArrayList(usize).init(allocator);
    defer return_types.deinit();
    const ops = seq.operateds.items;

    for (0..ops.len) |i| {
        const op = ops[ops.len - 1 - i];
        assert(!op.return_type.isImplicit());

        if (op.return_type.isNone())
            continue;

        try return_types.append(i);
    }

    seq.action = try ir.Action.returnBack(allocator, return_types.items);
}

/// linearize the grammar
///
/// needs:
///     * no resolved types
/// change bounds:
///     * no operated sequences
fn desugarSequences(self: *PegPassManager, def: *const ir.Definition, op: *ir.Operated) anyerror!?ir.Definition {
    return switch (op.value) {
        .SEQ => try self.extractDefinition(op, def.identifier),
        else => null,
    };
}

/// converts all operateds with a question operator
/// to add the corresponding sequence
///
/// needs: none
/// change bounds:
///     * no question operators for operateds
fn desugarQuestionOp(
    self: *PegPassManager,
    def: *const ir.Definition,
    op: *ir.Operated,
) anyerror!?ir.Definition {
    const allocator = self.ir.allocator;
    if (op.postfix_op != .QUESTION)
        return null;

    op.postfix_op = .NONE;
    var new_def = try self.extractDefinition(op, def.identifier);
    var empty_seq = try ir.Sequence.empty(allocator);
    empty_seq.action = if (new_def.return_type.isNone())
        ir.Action.empty(allocator)
    else
        try ir.Action.nullAction(allocator);
    try new_def.sequences.insert(0, empty_seq);

    return new_def;
}

/// converts all operateds with a plus operator to the corresponding new definition
///
/// needs:
///     * there are no sequences as operateds
/// change bounds:
///     * there are no plus operators for operateds
fn desugarPlusSequence(
    self: *PegPassManager,
    def: *const ir.Definition,
    seq: *ir.Sequence,
) anyerror!?ir.Definition {
    const ops = seq.operateds.items;
    const allocator = self.ir.allocator;
    for (ops) |*op| {
        switch (op.value) {
            .SEQ => unreachable,
            else => {},
        }

        if (op.postfix_op != .PLUS)
            continue;

        var new_op = try op.clone(allocator);
        new_op.postfix_op = .NONE;
        try self.setReturnType(&new_op);

        var new_def = try self.extractDefinition(op, def.identifier);

        const new_sequence = &new_def.sequences.items[0];
        try new_sequence.operateds.append(new_op);
        new_sequence.operateds.items[0].postfix_op = .STAR;

        if (new_def.return_type.isExistant()) {
            new_sequence.action.deinit(allocator);
            new_sequence.action = try ir.Action.popAddList(allocator, 1, 0);
        }

        assert(op.postfix_op == .NONE);
        return new_def;
    }

    return null;
}

/// a pass that removes epsilons from sequences
///
/// needs:
///     * everything that could desugar to epsilon should be desugared
/// change bounds:
///     * no epsilons left
///     (an epsilon is everything that matches a string of length 0)
fn removeEps(_: *PegPassManager, seq: *ir.Sequence) anyerror!void {
    var ops = &seq.operateds;
    var read: usize = 0;

    for (ops.items) |copy| {
        ops.items[read] = copy;
        const val = ops.items[read].value;

        switch (val) {
            .EPSILON => {},
            .LITERAL => |lit| if (lit.len != 0) {
                read += 1;
            },
            else => read += 1,
        }
    }

    ops.shrinkRetainingCapacity(read);
}

/// a pass that checks if nonterminals con be produced to nothing
///
/// needs:
///     * all epsilons should be removed
/// change bounds:
///     * the `accpets_eps` field is correctly set
fn setAcceptsEps(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    const seqs = def.sequences.items;
    outer: for (seqs) |seq| {
        const ops = seq.operateds.items;
        for (ops) |op| {
            switch (op.value) {
                .ID => |id| if (!self.getDefinition(id).accepts_eps) {
                    continue :outer;
                },
                .CUT => {},
                else => continue :outer,
            }
        }

        def.accepts_eps = true;
        return;
    }
}

/// extract the given operated into a new definition.
/// for it to work the types have to be assigned already to the operateds
///
/// there are two possible scenarios:
///     * the operated is a sequence:
///         in this case the sequence will be extraced and
///         the operated linking to the new definition will keep all operators
///     * the operated is not a sequence:
///         in this case the operated will be extracted with the postfix operator
///         the prefix operator will remain on the base
fn extractDefinition(
    self: *PegPassManager,
    op: *ir.Operated,
    name: []const u8,
) !ir.Definition {
    const allocator = self.ir.allocator;
    const new_seq = switch (op.value) {
        .SEQ => |*seq| blk: {
            assert(seq.action.isEmpty());
            var vals = std.ArrayList(usize).init(allocator);
            var all_impl = false;
            defer vals.deinit();
            for (seq.operateds.items, 0..) |val, i| {
                const ret_type = val.return_type;
                if (ret_type.isImplicit()) {
                    all_impl = true;
                } else if (!ret_type.isNone()) {
                    try vals.append(i);
                }
            }

            assert(!all_impl or vals.items.len == 0);
            seq.action = if (all_impl)
                ir.Action.impl()
            else
                try ir.Action.returnBack(allocator, vals.items);
            break :blk seq.*;
        },
        else => try ir.Sequence.init(
            allocator,
            &.{op.*},
            if (op.return_type.isImplicit())
                ir.Action.impl()
            else
                try ir.Action.returnBack(
                    allocator,
                    if (op.return_type.isNone()) &.{} else &.{0},
                ),
            op.string,
        ),
    };

    const return_types = try allocator.alloc(
        ir.ReturnType,
        new_seq.operateds.items.len,
    );
    defer allocator.free(return_types);

    for (new_seq.operateds.items, 0..) |ret, i| {
        return_types[i] = ret.return_type;
    }

    const return_type = try ir.ReturnType.tupelize(allocator, return_types);

    const defs = self.ir.defs.items;
    const def = try ir.Definition.init(
        allocator,
        &.{new_seq},
        name,
        return_type,
        defs.len,
    );
    op.* = ir.Operated.initId(
        defs.len,
        try return_type.clone(allocator),
        op.prefix_op,
        switch (op.value) {
            .SEQ => op.postfix_op,
            else => .NONE,
        },
        op.string,
    );

    return def;
}

/// currently not used
fn inlineDefs(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    var i: usize = 1;
    const allocator = self.ir.allocator;
    const seqs = &def.sequences;
    while (i <= seqs.items.len) : (i += 1) {
        const seq = &seqs.items[i - 1];
        var id: usize = 0;
        var index: usize = 0;
        var to_inline: *ir.Definition = undefined;
        var found = false;

        outer: for (seq.operateds.items, 0..) |op, j| {
            assert(op.postfix_op == .NONE);

            id = switch (op.value) {
                .ID => |ident| ident,
                .IDENTIFIER,
                .SEQ,
                .EPSILON,
                => unreachable,
                .DOT,
                .CLASS,
                .LITERAL,
                .CUT,
                => continue,
            };

            if (op.prefix_op != .NONE) continue;

            to_inline = self.getDefinition(id);
            if (to_inline.mid_recurse or to_inline.right_recurse) {
                continue;
            }

            for (to_inline.sequences.items) |sequence| {
                if (!sequence.action.isEmpty()) continue :outer;
            }

            index = j;
            found = true;
            break;
        }

        if (!found) continue;
        found = false;

        i -= 1;
        var base_seq = seq;
        var ref_op = base_seq.operateds.orderedRemove(index);
        ref_op.deinit(allocator);

        const inline_seqs = to_inline.sequences.items;

        const new_items = try seqs.addManyAt(i + 1, inline_seqs.len - 1);
        base_seq = &seqs.items[i];

        for (new_items) |*sequence| {
            sequence.* = try base_seq.clone(allocator);
        }

        for (seqs.items[i .. i + inline_seqs.len], inline_seqs) |*to, from| {
            try self.inlineSeq(to, from, index);
        }
    }
}

fn inlineOneSeqDefs(self: *PegPassManager, def: *ir.Definition) anyerror!void {
    var i: usize = 1;
    const seqs = &def.sequences;
    while (i <= seqs.items.len) : (i += 1) {
        const seq = &seqs.items[i - 1];
        var id: usize = 0;
        var index: usize = 0;
        var to_inline: *ir.Definition = undefined;
        var found = false;

        for (seq.operateds.items, 0..) |op, j| {
            assert(op.postfix_op == .NONE);

            id = switch (op.value) {
                .ID => |ident| ident,
                .IDENTIFIER,
                .SEQ,
                .EPSILON,
                => unreachable,
                .DOT,
                .CLASS,
                .LITERAL,
                .CUT,
                => continue,
            };

            if (op.prefix_op != .NONE) continue;

            to_inline = self.getDefinition(id);

            const inline_seqs = to_inline.sequences.items;
            if (inline_seqs.len != 1) continue;
            if (inline_seqs[0].action.use_match) continue;

            index = j;
            found = true;
            break;
        }

        if (!found) continue;
        found = false;

        i -= 1;
        var var_name: usize = undefined;
        const op_to_look_for = &seq.operateds.items[index];
        for (seq.action.rets.items) |ret| {
            if (ret.ret_op != op_to_look_for) continue;

            var_name = ret.var_name;
            found = true;
            break;
        }
        assert(found);
        found = false;

        const inline_seq = &to_inline.sequences.items[0];
        try self.inlineSeq(seq, inline_seq, index, var_name);
    }
}

fn inlineSeq(
    self: *PegPassManager,
    to: *ir.Sequence,
    from: *const ir.Sequence,
    replace: usize,
    var_name: usize,
) !void {
    const allocator = self.ir.allocator;
    var clone = try from.clone(allocator);
    defer {
        clone.operateds.clearRetainingCapacity();
        clone.deinit(allocator);
    }

    var ref_op = to.operateds.orderedRemove(replace);
    ref_op.deinit(allocator);

    const base_ptr = to.operateds.items.ptr;
    try to.operateds.insertSlice(replace, clone.operateds.items);

    // move the ret ptrs to the new ops
    for (to.action.rets.items) |*ret| {
        const old_index = @divExact(
            @intFromPtr(ret.ret_op) - @intFromPtr(base_ptr),
            @sizeOf(ir.Operated),
        );
        const new_index = old_index +
            @as(usize, if (old_index >= replace) clone.operateds.items.len else 1) - 1;
        ret.ret_op = &to.operateds.items[new_index];
    }

    try self.inlineAction(
        &to.action,
        clone.action,
        clone.operateds.items,
        to.operateds.items[replace .. replace + from.operateds.items.len],
        var_name,
    );
}

fn inlineAction(
    self: *PegPassManager,
    to: *ir.Action,
    from: ir.Action,
    old_ops: []const ir.Operated,
    added_ops: []const ir.Operated,
    id: usize,
) !void {
    const allocator = self.ir.allocator;
    const to_empty = to.isEmpty();
    var found = false;
    var index: usize = undefined;
    var max_id: usize = 0;

    assert(!from.implicit and !to.implicit);
    assert(!from.use_match);

    for (to.rets.items, 0..) |*ret, i| {
        max_id = @max(ret.var_name, max_id);
        if (ret.var_name != id) continue;
        found = true;
        index = i;
    }
    assert(found);

    for (to.bases.items) |base| {
        if (base.id == 0) continue;
        max_id = @max(base.id - 1, max_id);
    }

    // add the returns
    const from_rets = from.rets.items;
    if (from_rets.len == 0) {
        _ = to.rets.orderedRemove(index);
    } else {
        _ = try to.rets.addManyAt(index, from_rets.len - 1);
    }

    const append_slice = to.rets.items[index .. index + from_rets.len];
    for (from_rets, append_slice) |from_ret, *to_ret| {
        const op_index = @divExact(
            @intFromPtr(from_ret.ret_op) - @intFromPtr(old_ops.ptr),
            @sizeOf(ir.Operated),
        );

        assert(&old_ops[op_index] == from_ret.ret_op);
        assert(switch (added_ops[op_index].value) {
            .ID => true,
            else => false,
        });
        to_ret.* = .{
            .var_name = max_id + 1 + from_ret.var_name,
            .ret_op = &added_ops[op_index],
        };
    }

    // add the bases
    for (from.bases.items) |base| {
        var b_clone = try base.clone(allocator);
        b_clone.id = if (b_clone.id == 0 and to_empty)
            0
        else if (b_clone.id == 0)
            id + 1
        else
            max_id + 1 + base.id;

        for (b_clone.action_vars.items) |*a_var| {
            if (a_var.arg_num) |*num| {
                num.* += max_id + 1;
            }
        }

        try to.bases.append(b_clone);
    }
}

fn defAppendPass(
    self: *PegPassManager,
    comptime pass: fn (self: *PegPassManager, def: *ir.Definition) anyerror!?ir.Definition,
    comptime name: []const u8,
) !void {
    var progress = self.main_progress.start(name, 0);
    defer progress.end();
    var defs = &self.ir.defs;
    var i: usize = 0;

    while (i < defs.items.len) {
        const def = &defs.items[i];
        const result = try pass(self, def);

        if (result) |new_def| {
            try defs.append(new_def);
        } else {
            i += 1;
        }
    }
}

fn defPass(
    self: *PegPassManager,
    comptime backwards: bool,
    comptime pass: fn (self: *PegPassManager, def: *ir.Definition) anyerror!void,
    comptime name: []const u8,
) !void {
    var progress = self.main_progress.start(name, 0);
    defer progress.end();
    const defs = self.ir.defs.items;
    if (backwards) {
        for (0..defs.len) |i| {
            const def = &defs[defs.len - 1 - i];
            try pass(self, def);
        }
    } else {
        for (defs) |*def| {
            try pass(self, def);
        }
    }
}

fn seqPass(
    self: *PegPassManager,
    comptime pass: fn (self: *PegPassManager, seq: *ir.Sequence) anyerror!void,
    comptime name: []const u8,
) !void {
    const Iter = struct {
        pub fn sequence_iter(s: *PegPassManager, def: *ir.Definition) anyerror!void {
            for (def.sequences.items) |*seq| {
                try pass(s, seq);
            }
        }
    };

    try self.defPass(false, Iter.sequence_iter, name);
}

fn seqAppendDefPass(
    self: *PegPassManager,
    comptime pass: fn (
        self: *PegPassManager,
        def: *const ir.Definition,
        seq: *ir.Sequence,
    ) anyerror!?ir.Definition,
    comptime name: []const u8,
) !void {
    const Iter = struct {
        pub fn sequence_iter(s: *PegPassManager, def: *ir.Definition) anyerror!?ir.Definition {
            for (def.sequences.items) |*seq| {
                if (try pass(s, def, seq)) |new_def| {
                    return new_def;
                }
            }

            return null;
        }
    };

    try self.defAppendPass(Iter.sequence_iter, name);
}

fn seqAppendPass(
    self: *PegPassManager,
    comptime pass: fn (self: *PegPassManager, seq: *ir.Sequence) anyerror!?ir.Sequence,
    comptime name: []const u8,
) !void {
    const Iter = struct {
        pub fn sequence_iter(s: *PegPassManager, def: *ir.Definition) anyerror!void {
            var i: usize = 0;
            while (i < def.sequences.items.len) {
                const seq = &def.sequences.items[i];
                const result = try pass(s, seq);

                if (result) |new_sequence| {
                    try def.sequences.insert(i, new_sequence);
                } else {
                    i += 1;
                }
            }
        }
    };

    try self.defPass(false, Iter.sequence_iter, name);
}

fn opAppendDefPass(
    self: *PegPassManager,
    comptime pass: fn (
        self: *PegPassManager,
        def: *const ir.Definition,
        op: *ir.Operated,
    ) anyerror!?ir.Definition,
    comptime name: []const u8,
) !void {
    const Iter = struct {
        pub fn sequence_iter(
            s: *PegPassManager,
            def: *const ir.Definition,
            seq: *ir.Sequence,
        ) anyerror!?ir.Definition {
            for (seq.operateds.items) |*op| {
                if (try pass(s, def, op)) |new_def| {
                    return new_def;
                }
            }

            return null;
        }
    };

    try self.seqAppendDefPass(Iter.sequence_iter, name);
}

fn opPass(
    self: *PegPassManager,
    comptime pass: fn (self: *PegPassManager, op: *ir.Operated) anyerror!void,
    comptime name: []const u8,
) !void {
    const Iter = struct {
        pub fn op_iter(s: *PegPassManager, seq: *ir.Sequence) anyerror!void {
            for (seq.operateds.items) |*op| {
                try pass(s, op);
            }
        }
    };

    try self.seqPass(Iter.op_iter, name);
}

/// set the return type for all operateds
///
/// needs:
///     * all definitions have the right return type
///     * all identifiers are ids
///     * there are no operated sequences
/// change bounds:
///     * the given operated is typed
fn setReturnType(self: *PegPassManager, op: *ir.Operated) anyerror!void {
    const allocator = self.ir.allocator;

    op.return_type.deinit(allocator);
    op.return_type = switch (op.value) {
        .ID => |id| blk: {
            const def = self.getDefinition(id);
            assert(!def.return_type.isImplicit());
            if (op.prefix_op != .NONE) {
                break :blk ir.ReturnType.empty();
            }

            if (op.postfix_op == .QUESTION) {
                break :blk try ir.ReturnType.nullablize(
                    allocator,
                    def.return_type,
                );
            }

            break :blk if (op.postfix_op != .NONE)
                try ir.ReturnType.listify(allocator, &def.return_type)
            else
                try def.return_type.clone(allocator);
        },
        .IDENTIFIER,
        .SEQ,
        => unreachable,
        else => ir.ReturnType.empty(),
    };
}

fn typeError(
    self: *PegPassManager,
    current_op: *ir.Operated,
    new_op: *ir.Operated,
) !void {
    self.pass = false;
    const allocator = self.ir.allocator;
    const errMsg = try std.fmt.allocPrint(
        allocator,
        "{s}",
        .{new_op.return_type.value.?},
    );
    defer allocator.free(errMsg);
    const infoMsg = try std.fmt.allocPrint(
        allocator,
        "{s}",
        .{current_op.return_type.value.?},
    );
    defer allocator.free(infoMsg);

    try Error.print(
        "unknown return type",
        self.ir.file_name,
        self.ir.chars,
        &.{
            .{ .place = new_op.value.getChars(), .msg = errMsg, .t = .ERROR },
            .{ .place = current_op.value.getChars(), .msg = infoMsg, .t = .INFO },
        },
    );
}

fn getDefinition(self: *PegPassManager, id: usize) *ir.Definition {
    return &self.ir.defs.items[id];
}
