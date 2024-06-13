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
const su = @import("string_utils.zig");

pub const PegIr = struct {
    top_level_comment: []const []const u8,
    name: []const u8,
    file_name: []const u8,
    top_header: []const u8,
    field_header: []const u8,
    defs: std.ArrayList(Definition),
    allocator: Allocator,
    chars: []const u8,
    is_acceptor: bool,

    pub fn init(allocator: Allocator) PegIr {
        return .{
            .top_level_comment = &.{},
            .name = "",
            .top_header = "",
            .field_header = "",
            .defs = std.ArrayList(Definition).init(allocator),
            .allocator = allocator,
            .chars = "",
            .file_name = "",
            .is_acceptor = false,
        };
    }

    pub fn clone(self: PegIr) !PegIr {
        var defs = std.ArrayList(Definition).init(self.allocator);

        for (self.defs.items) |def| {
            try defs.append(try def.clone(self.allocator));
        }

        return .{
            .top_level_comment = try self.allocator.dupe([]const u8, self.top_level_comment),
            .name = self.name,
            .file_name = self.file_name,
            .top_header = self.top_header,
            .field_header = self.field_header,
            .grammar = try self.grammar.clone(self.allocator),
            .allocator = self.allocator,
            .chars = self.chars,
            .is_acceptor = self.is_acceptor,
        };
    }

    pub fn deinit(self: PegIr) void {
        for (self.defs.items) |*def| {
            def.deinit(self.allocator);
        }

        self.defs.deinit();
        self.allocator.free(self.top_level_comment);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("acceptor: {}\n", .{self.is_acceptor});
        try writer.print(
            "NAME: {s}\nTOP START:\n{s}\nTOP END\nFIELD START:\n{s}FIELD END\n\n",
            .{
                self.name,
                self.top_header,
                self.field_header,
            },
        );
        for (self.defs.items) |def| {
            try writer.print("{s}\n", .{def});
        }
    }
};

pub const ReturnType = union(enum) {
    list: *ReturnType,
    tuple: std.ArrayList(ReturnType),
    nullable: *ReturnType,
    base: struct {
        value: ?[]const u8,
        implicit: bool,
        owned: bool,
    },

    pub fn init(value: ?[]const u8, owned: bool, implicit: bool) ReturnType {
        assert(implicit or value != null);

        return .{ .base = .{
            .value = value,
            .owned = owned,
            .implicit = implicit,
        } };
    }

    /// returns an empty return type
    pub fn empty() ReturnType {
        return .{ .base = .{
            .owned = false,
            .value = null,
            .implicit = false,
        } };
    }

    /// returns an implicit return type
    pub fn impl() ReturnType {
        return .{ .base = .{
            .owned = false,
            .value = null,
            .implicit = true,
        } };
    }

    /// returns a list form of the given return type
    pub fn listify(allocator: Allocator, t: *ReturnType) !ReturnType {
        if (t.isImplicit()) return ReturnType.impl();
        if (t.isNone()) return ReturnType.empty();

        return .{
            .list = try t.clonePtr(allocator),
        };
    }

    /// returns the nullable version of the given return type
    pub fn nullablize(allocator: Allocator, t: ReturnType) !ReturnType {
        if (t.isImplicit()) return ReturnType.impl();
        if (t.isNone()) return ReturnType.empty();

        return .{
            .nullable = try t.clonePtr(allocator),
        };
    }

    pub fn isImplicit(self: ReturnType) bool {
        return switch (self) {
            .base => |val| val.implicit,
            else => false,
        };
    }

    pub fn isExistant(self: ReturnType) bool {
        return switch (self) {
            .base => |val| val.value != null,
            else => true,
        };
    }

    /// returns a tuple form of the given return types
    pub fn tupelize(
        allocator: Allocator,
        types: []const ReturnType,
    ) !ReturnType {
        var non_empty = std.ArrayList(ReturnType).init(allocator);
        var all_impl = false;

        for (types) |t| {
            if (t.isImplicit()) {
                all_impl = true;
            } else if (!t.isNone()) {
                try non_empty.append(t);
            }
        }

        assert(!all_impl or non_empty.items.len == 0);

        if (non_empty.items.len > 1) {
            for (non_empty.items) |*val| {
                val.* = try val.clone(allocator);
            }

            return .{ .tuple = non_empty };
        }

        defer non_empty.deinit();

        if (all_impl) return ReturnType.impl();
        return if (non_empty.items.len == 0)
            ReturnType.empty()
        else
            try non_empty.items[0].clone(allocator);
    }

    /// check if the return type is none (or void)
    pub fn isNone(self: ReturnType) bool {
        return switch (self) {
            .base => |val| val.value == null and !val.implicit,
            else => false,
        };
    }

    pub fn getString(self: ReturnType) ?[]const u8 {
        return switch (self) {
            .base => |b| b.value,
            else => null,
        };
    }

    pub fn deinit(self: *ReturnType, allocator: Allocator) void {
        switch (self.*) {
            .base => |*b| {
                assert(!b.implicit or b.value == null);
                assert(!b.owned or b.value != null);
                if (b.owned) {
                    allocator.free(b.value.?);
                    b.owned = false;
                    b.value = null;
                }
            },
            .tuple => |*list| {
                for (list.items) |*ret| {
                    ret.deinit(allocator);
                }
                list.deinit();
            },
            .nullable,
            .list,
            => |ref| {
                ref.deinit(allocator);
                allocator.destroy(ref);
            },
        }
    }

    pub fn clone(self: ReturnType, allocator: Allocator) !ReturnType {
        return switch (self) {
            .base => |b| blk: {
                assert(!b.implicit or b.value == null);
                assert(!b.owned or b.value != null);
                break :blk if (b.owned) in: {
                    break :in .{ .base = .{
                        .value = if (b.value) |val|
                            try allocator.dupe(u8, val)
                        else
                            unreachable,
                        .implicit = b.implicit,
                        .owned = b.owned,
                    } };
                } else self;
            },
            .tuple => |list| blk: {
                var new_list = std.ArrayList(ReturnType).init(allocator);
                for (list.items) |item| {
                    try new_list.append(try item.clone(allocator));
                }
                break :blk .{ .tuple = new_list };
            },
            .nullable,
            => |ref| .{ .nullable = try ref.clonePtr(allocator) },
            .list,
            => |ref| .{ .list = try ref.clonePtr(allocator) },
        };
    }

    pub fn clonePtr(
        self: ReturnType,
        allocator: Allocator,
    ) anyerror!*ReturnType {
        const new = try allocator.create(ReturnType);
        new.* = try self.clone(allocator);
        return new;
    }

    pub fn format(
        self: ReturnType,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (self.isImplicit()) {
            try writer.print("impl", .{});
            return;
        }

        switch (self) {
            .base => |s| if (s.value) |val| {
                try writer.print("{s}", .{val});
            } else {
                try writer.print("void", .{});
            },
            .tuple => |list| {
                try writer.print("{s}", .{"std.meta.Tuple(&.{"});
                for (list.items) |item| {
                    try writer.print("{s},", .{item});
                }
                try writer.print("{s}", .{"})"});
            },
            .nullable => |t| {
                try writer.print("?{s}", .{t});
            },
            .list => |t| {
                try writer.print("std.ArrayList({s})", .{t});
            },
        }
    }
};

pub const Definition = struct {
    identifier: []const u8,
    return_type: ReturnType,
    sequences: std.ArrayList(Sequence),
    id: usize,
    accepts_eps: bool,
    mid_recurse: bool,
    right_recurse: bool,
    regular: bool,
    finite: bool,
    moves_actions: bool,
    is_terminal: bool,

    pub fn init(
        allocator: Allocator,
        seqs: []const Sequence,
        identifier: []const u8,
        return_type: ReturnType,
        id: usize,
    ) !Definition {
        var sequences = std.ArrayList(Sequence).init(allocator);
        try sequences.appendSlice(seqs);

        return .{
            .id = id,
            .identifier = identifier,
            .return_type = return_type,
            .sequences = sequences,
            .mid_recurse = false,
            .accepts_eps = false,
            .right_recurse = false,
            .regular = true,
            .finite = true,
            .moves_actions = false,
            .is_terminal = false,
        };
    }

    pub fn clone(self: Definition, allocator: Allocator) !Definition {
        var new_seqs = std.ArrayList(Sequence).init(allocator);
        for (self.sequences.items) |seq| {
            try new_seqs.append(seq.clone());
        }

        return .{
            .identifier = self.identifier,
            .return_type = try self.return_type.clone(allocator),
            .sequences = new_seqs,
        };
    }

    pub fn deinit(self: *Definition, allocator: Allocator) void {
        for (self.sequences.items) |seq| {
            seq.deinit(allocator);
        }

        self.sequences.deinit();
        self.return_type.deinit(allocator);
    }

    pub fn generated(self: *Definition) bool {
        return self.identifier.len == 0;
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} (%{d}){s}{s}{s}{s}{s}", .{
            self.identifier,
            self.id,
            if (self.accepts_eps) " (ε)" else "",
            if (self.mid_recurse) " (m∞)" else if (self.right_recurse) " (r∞)" else "",
            if (self.regular) " (reg)" else if (self.finite) " (fin)" else "",
            if (self.moves_actions) " (act)" else "",
            if (self.is_terminal) " (ter)" else "",
        });
        try writer.print(": {s}", .{self.return_type});
        try writer.print(" =\n", .{});

        for (0..self.sequences.items.len) |i| {
            const seq = self.sequences
                .items[self.sequences.items.len - 1 - i];
            try writer.print("\t| {s}\n", .{seq});
        }
    }
};

pub const Sequence = struct {
    operateds: std.ArrayList(Operated),
    action: Action,
    string: []const u8,

    pub fn init(
        allocator: Allocator,
        ops: []const Operated,
        action: Action,
        string: []const u8,
    ) !Sequence {
        var operateds = std.ArrayList(Operated).init(allocator);
        try operateds.appendSlice(ops);

        return .{
            .operateds = operateds,
            .action = action,
            .string = string,
        };
    }

    pub fn empty(allocator: Allocator) !Sequence {
        return try Sequence.init(
            allocator,
            &.{},
            Action.empty(allocator),
            "",
        );
    }

    pub fn remove(self: *Sequence, i: usize, allocator: Allocator) !void {
        var to_delete = self.operateds.orderedRemove(i);
        to_delete.deinit(allocator);
    }

    pub fn clone(self: Sequence, allocator: Allocator) anyerror!Sequence {
        var new_ops = std.ArrayList(Operated).init(allocator);
        for (self.operateds.items) |op| {
            try new_ops.append(try op.clone(allocator));
        }

        return .{
            .operateds = new_ops,
            .action = try self.action.clone(
                allocator,
                self.operateds.items,
                new_ops.items,
            ),
            .string = self.string,
        };
    }

    pub fn deinit(self: Sequence, allocator: Allocator) void {
        for (self.operateds.items) |*op| {
            op.deinit(allocator);
        }

        self.operateds.deinit();
        self.action.deinit(allocator);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        for (0..self.operateds.items.len) |i| {
            const op = self.operateds.items[
                self.operateds.items.len - 1 - i
            ];
            try writer.print("{s} ", .{op});
        }

        try writer.print(" {s}", .{self.action});
    }
};

pub const ActionReturn = struct {
    var_name: usize,
    ret_op: *const Operated,

    pub fn init(name: usize, op: *Operated) ActionReturn {
        return .{
            .var_name = name,
            .ret_op = op,
        };
    }

    pub fn clone(
        self: ActionReturn,
        old_ops: []const Operated,
        new_ops: []const Operated,
    ) ActionReturn {
        const index = @divExact(
            @intFromPtr(self.ret_op) - @intFromPtr(old_ops.ptr),
            @sizeOf(Operated),
        );

        return .{
            .var_name = self.var_name,
            .ret_op = &new_ops[index],
        };
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("({d}: {}) ", .{ self.var_name, self.ret_op.return_type });
    }
};

pub const ActionBase = struct {
    data: []const u8,
    owned: bool,
    action_vars: std.ArrayList(ActionVar),
    return_type: ReturnType,

    /// the id is the name of the implied action var
    /// when 0 it is the return var
    /// (the indexes are ofsetted by 1)
    id: usize,
    mut: bool,

    pub fn deinit(self: *ActionBase, allocator: Allocator) void {
        if (self.owned) {
            allocator.free(self.data);
        }

        self.action_vars.deinit();
        self.return_type.deinit(allocator);
    }

    pub fn clone(self: ActionBase, allocator: Allocator) !ActionBase {
        const new_string = try allocator.dupe(u8, self.data);
        var action_vars = std.ArrayList(ActionVar).init(allocator);

        for (self.action_vars.items) |a_var| {
            try action_vars.append(a_var.clone(self.data, new_string));
        }

        return .{
            .data = new_string,
            .owned = true,
            .action_vars = action_vars,
            .return_type = try self.return_type.clone(allocator),
            .id = self.id,
            .mut = self.mut,
        };
    }

    pub fn getNext(self: ActionBase, index: usize) ?ActionVar {
        var smallest_delta: usize = self.data.len + 1;
        var closest_var: ?ActionVar = null;
        for (self.action_vars.items) |action_var| {
            const delta = @intFromPtr(action_var.var_decl.ptr) -
                @intFromPtr(self.data.ptr);

            if (delta < index or delta > smallest_delta) {
                continue;
            }

            smallest_delta = delta;
            closest_var = action_var;
        }

        return closest_var;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.id == 0) {
            try writer.print("ret = ", .{});
        } else {
            try writer.print("{d} = ", .{self.id - 1});
        }
        for (self.action_vars.items) |a_var| {
            try writer.print("{d}, ", .{a_var.arg_num.?});
        }
    }
};

pub const Action = struct {
    implicit: bool,
    use_match: bool,
    rets: std.ArrayList(ActionReturn),
    bases: std.ArrayList(ActionBase),

    var empty_alloc = std.heap.FixedBufferAllocator.init(&.{});

    pub fn clone(
        self: Action,
        allocator: Allocator,
        old_ops: []const Operated,
        new_ops: []const Operated,
    ) !Action {
        var bases = std.ArrayList(ActionBase).init(allocator);

        for (self.bases.items) |base| {
            try bases.append(try base.clone(allocator));
        }
        var rets = std.ArrayList(ActionReturn).init(allocator);
        for (self.rets.items) |ret| {
            try rets.append(ret.clone(old_ops, new_ops));
        }

        return .{
            .implicit = self.implicit,
            .use_match = self.use_match,
            .rets = rets,
            .bases = bases,
        };
    }

    pub fn init(
        allocator: Allocator,
        data: []const u8,
        owned: bool,
        implicit: bool,
        actions: []const ActionVar,
    ) !Action {
        var action_list = std.ArrayList(ActionVar).init(allocator);
        try action_list.appendSlice(actions);
        var use_match = false;
        for (action_list.items) |item| {
            if (!item.isMatchedString()) continue;

            use_match = true;
            break;
        }

        var bases = std.ArrayList(ActionBase).init(allocator);
        try bases.append(.{
            .data = data,
            .owned = owned,
            .action_vars = action_list,
            .return_type = ReturnType.impl(),
            .mut = false,
            .id = 0,
        });

        return .{
            .implicit = implicit,
            .use_match = use_match,
            .rets = std.ArrayList(ActionReturn).init(allocator),
            .bases = bases,
        };
    }

    /// returns an empty action
    /// `allocator` is needed for the rets
    pub fn empty(allocator: Allocator) Action {
        return .{
            .use_match = false,
            .implicit = false,
            .bases = std.ArrayList(ActionBase).init(allocator),
            .rets = std.ArrayList(ActionReturn).init(allocator),
        };
    }

    /// an action that returns null
    /// `allocator` is needed for the rets
    pub fn nullAction(allocator: Allocator) !Action {
        var bases = std.ArrayList(ActionBase).init(allocator);
        try bases.append(.{
            .data = "null",
            .owned = false,
            .action_vars = std.ArrayList(ActionVar).init(allocator),
            .return_type = ReturnType.impl(),
            .id = 0,
            .mut = false,
        });

        return .{
            .use_match = false,
            .implicit = false,
            .rets = std.ArrayList(ActionReturn).init(allocator),
            .bases = bases,
        };
    }

    /// returns an implicit action
    pub fn impl() Action {
        return .{
            .use_match = false,
            .implicit = true,
            .rets = std.ArrayList(ActionReturn)
                .init(empty_alloc.allocator()),
            .bases = std.ArrayList(ActionBase)
                .init(empty_alloc.allocator()),
        };
    }

    /// returns an action that appends an element to a list
    /// (useful for right recursion on lists)
    pub fn popAddList(
        allocator: Allocator,
        list_num: usize,
        add_num: usize,
    ) !Action {
        const str = "blk: {try $2.append($1); break :blk $2; }";
        const vars = &.{
            ActionVar.initVar(str[10..12], list_num, true),
            ActionVar.initVar(str[20..22], add_num, false),
            ActionVar.initVar(str[36..38], list_num, true),
        };

        return Action.init(allocator, str, false, false, vars);
    }

    /// returns an action that returns the args as a tuple or
    /// just one arg (or nothing) depending on the amount of args
    pub fn returnBack(allocator: Allocator, args: []const usize) !Action {
        if (args.len == 0) {
            return Action.empty(allocator);
        }

        if (args.len == 1) {
            const str = "$1";
            return Action.init(allocator, str, false, false, &.{
                ActionVar.initVar(str[0..], args[0], false),
            });
        }

        const str = try std.fmt.allocPrint(
            allocator,
            ".{{ {s} }}",
            .{su.times("?,", args.len)},
        );
        const BaseOffset = 3;

        const vars = try allocator.alloc(ActionVar, args.len);
        defer allocator.free(vars);
        for (vars, args, 0..) |*val, arg, i| {
            val.* = ActionVar.initVar(
                str[BaseOffset + 2 * i .. BaseOffset + 2 * i + 1],
                arg,
                false,
            );
        }

        return Action.init(allocator, str, true, false, vars);
    }

    pub fn setRets(self: *Action, seq: Sequence) !void {
        assert(!self.implicit);

        const ops = seq.operateds.items;
        for (0..ops.len) |i| {
            const op = &ops[ops.len - 1 - i];

            assert(!op.return_type.isImplicit());
            // the reasoning is to contain everything
            // that could trigger an action
            switch (op.value) {
                .ID => try self.rets.append(ActionReturn.init(i, op)),
                else => {},
            }
        }
    }

    pub fn deinit(self: Action, allocator: Allocator) void {
        for (self.bases.items) |*base| {
            base.deinit(allocator);
        }
        self.bases.deinit();
        self.rets.deinit();
    }

    pub fn isEmpty(self: Action) bool {
        return self.bases.items.len == 0 and !self.implicit;
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        if (self.isEmpty()) return;

        if (self.implicit) {
            try writer.print("{{ infer }}", .{});
            return;
        }

        try writer.print("{{ {s} }}", .{self.bases.items[0].data});
    }
};

pub const ActionVar = struct {
    // the slice containing the declaration of the var in the action
    var_decl: []const u8,

    // the argument number
    // if the argument is the current string or an escape
    // this field should be null
    arg_num: ?usize,

    // a string preceded with an escape character
    // if the arg num exists true means that the arg is mutable
    escape: bool,

    pub fn initVar(
        decl: []const u8,
        num: usize,
        mut: bool,
    ) ActionVar {
        return .{
            .var_decl = decl,
            .arg_num = num,
            .escape = mut,
        };
    }

    pub fn clone(
        self: ActionVar,
        old_base: []const u8,
        new_base: []const u8,
    ) ActionVar {
        assert(old_base.len == new_base.len);

        const decl_start = @intFromPtr(self.var_decl.ptr) - @intFromPtr(old_base.ptr);
        assert(decl_start + self.var_decl.len <= old_base.len);

        return .{
            .var_decl = new_base[decl_start .. decl_start + self.var_decl.len],
            .arg_num = self.arg_num,
            .escape = self.escape,
        };
    }

    /// true iff the action var is an escape sequence
    pub fn isEscape(self: ActionVar) bool {
        return self.escape and self.arg_num == null;
    }

    /// true iff the action var returns the matched string
    /// to the current sequence
    pub fn isMatchedString(self: ActionVar) bool {
        return !self.escape and self.arg_num == null;
    }

    /// true iff the var is used as constant
    pub fn isConstVar(self: ActionVar) bool {
        return !self.escape and self.arg_num != null;
    }

    /// true iff the var can be mutated
    pub fn isMutVar(self: ActionVar) bool {
        return self.escape and self.arg_num != null;
    }
};

pub const Operated = struct {
    prefix_op: PrefixOp,
    postfix_op: PostfixOp,
    value: Primary,
    return_type: ReturnType,
    string: []const u8,

    pub fn initId(
        id: usize,
        return_type: ReturnType,
        prefix: PrefixOp,
        postfix: PostfixOp,
        string: []const u8,
    ) Operated {
        return .{
            .string = string,
            .prefix_op = prefix,
            .postfix_op = postfix,
            .return_type = return_type,
            .value = .{ .ID = id },
        };
    }

    pub fn deinit(self: *Operated, allocator: Allocator) void {
        self.value.deinit(allocator);
        self.return_type.deinit(allocator);
    }

    pub fn clone(self: Operated, allocator: Allocator) !Operated {
        return .{
            .string = self.string,
            .prefix_op = self.prefix_op,
            .postfix_op = self.postfix_op,
            .value = try self.value.clone(allocator),
            .return_type = try self.return_type.clone(allocator),
        };
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("({s}{s}{s}: {s})", .{
            self.prefix_op,
            self.value,
            self.postfix_op,
            self.return_type,
        });
    }
};

pub const PrefixOp = enum(u8) {
    NONE = 0,
    AND,
    NOT,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{switch (self) {
            .NONE => "",
            .AND => "&",
            .NOT => "!",
        }});
    }
};

pub const PostfixOp = enum(u8) {
    NONE = 0,
    QUESTION,
    STAR,
    PLUS,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{switch (self) {
            .NONE => "",
            .QUESTION => "?",
            .STAR => "*",
            .PLUS => "+",
        }});
    }
};

pub const Primary = union(enum) {
    IDENTIFIER: []const u8,
    ID: usize,
    LITERAL: []const u8,
    CLASS: Class,
    SEQ: Sequence,
    DOT,
    EPSILON,
    CUT,

    pub fn deinit(self: Primary, allocator: Allocator) void {
        switch (self) {
            .CLASS => |v| v.deinit(),
            .SEQ => |v| v.deinit(allocator),
            .LITERAL => |lit| allocator.free(lit),
            else => {},
        }
    }

    pub fn getChars(self: Primary) []const u8 {
        return switch (self) {
            .IDENTIFIER => |ident| ident,
            .SEQ => |seq| seq.string,
            else => "",
        };
    }

    pub fn clone(self: Primary, allocator: Allocator) !Primary {
        return switch (self) {
            .DOT,
            .ID,
            .EPSILON,
            .CUT,
            .IDENTIFIER,
            => self,
            .LITERAL => |lit| .{
                .LITERAL = try allocator.dupe(u8, lit),
            },
            .CLASS => |c| .{
                .CLASS = try c.clone(),
            },
            .SEQ => |s| .{
                .SEQ = try s.clone(allocator),
            },
        };
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .DOT => try writer.print(".", .{}),
            .ID => |id| try writer.print("%{d}", .{id}),
            .EPSILON => try writer.print("ε", .{}),
            .IDENTIFIER => |string| try writer.print("{s}", .{
                string,
            }),
            .LITERAL => |string| try su.writeString(writer, string),
            .CLASS => |value| try writer.print("{s}", .{
                value,
            }),
            .SEQ => |value| try writer.print("({s})", .{
                value,
            }),
            .CUT => try writer.print("^", .{}),
        }
    }
};

pub const Class = struct {
    content: std.ArrayList(Range),

    pub fn init(allocator: Allocator) Class {
        return .{
            .content = std.ArrayList(Range).init(allocator),
        };
    }

    pub fn deinit(self: Class) void {
        self.content.deinit();
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("[", .{});
        for (self.content.items, 0..) |_, i| {
            const range = self.content.items[self.content.items.len - 1 - i];
            try writer.print("{s}, ", .{range});
        }
        try writer.print("]", .{});
    }

    pub fn clone(self: Class) !Class {
        return .{
            .content = try self.content.clone(),
        };
    }
};

pub const Range = struct {
    from: u8,
    to: u8,
    backing: []const u8,

    pub fn isChar(self: Range) bool {
        return self.to == 0;
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try su.writeChar(writer, self.from);
        if (self.isChar()) return;

        try writer.print("...", .{});
        try su.writeChar(writer, self.to);
    }
};
