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
const Writer = std.fs.File.Writer;

pub fn printDelim(
    comptime T: type,
    comptime delim: []const u8,
    values: []const T,
) DelimPrinter(T, delim) {
    return DelimPrinter(T, delim).init(values);
}

pub fn times(string: []const u8, n: usize) Times {
    return Times.init(string, n);
}

fn DelimPrinter(comptime T: type, comptime delimiter: []const u8) type {
    return struct {
        vals: []const T,

        const Self = @This();
        pub fn init(values: []const T) Self {
            return .{
                .vals = values,
            };
        }

        pub fn format(
            self: Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (self.vals.len == 0) return;
            try writer.print("{s}", .{self.vals[0]});
            for (self.vals[1..]) |val| {
                try writer.print("{s}", .{delimiter});
                try writer.print("{s}", .{val});
            }
        }
    };
}

pub fn toLower(string: []const u8) LowerCasePrinter {
    return .{ .string = string };
}

pub const LowerCasePrinter = struct {
    string: []const u8,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.string) |char| {
            switch (char) {
                'A'...'Z' => try writer.print("{c}", .{char - 'A' + 'a'}),
                else => try writer.print("{c}", .{char}),
            }
        }
    }
};

pub fn writeChar(writer: anytype, char: u8) !void {
    try writer.print("'", .{});
    try writeCharacter(writer, char);
    try writer.print("'", .{});
}

pub fn writeEscapeChar(writer: anytype, char: u8) !void {
    try writer.print("'", .{});
    try writeEscapeCharacter(writer, char);
    try writer.print("'", .{});
}

pub fn writeEscapeString(writer: anytype, string: []const u8) !void {
    try writer.print("\\\"", .{});
    for (string) |char| try writeEscapeCharacter(writer, char);
    try writer.print("\\\"", .{});
}

pub fn writeString(writer: anytype, string: []const u8) !void {
    try writer.print("\"", .{});
    for (string) |char| try writeCharacter(writer, char);
    try writer.print("\"", .{});
}

pub fn writeEscapeCharacter(writer: anytype, char: u8) !void {
    try writeCharBase(writer, char, "\\");
}

pub fn writeCharacter(writer: anytype, char: u8) !void {
    try writeCharBase(writer, char, "");
}

fn writeCharBase(writer: anytype, char: u8, escape: []const u8) !void {
    switch (char) {
        '\n' => try writer.print("{s}\\n", .{escape}),
        '\t' => try writer.print("{s}\\t", .{escape}),
        '\r' => try writer.print("{s}\\r", .{escape}),
        '\'',
        '"',
        '\\',
        => try writer.print("{s}{s}\\{c}", .{ escape, escape, char }),
        else => if ((char < 33 or char >= 127) and char != ' ')
            try writer.print("{s}\\x{x:0>2}", .{ escape, char })
        else
            try writer.print("{c}", .{char}),
    }
}

pub fn isScreamingSnakeCase(string: []const u8) bool {
    switch (string[0]) {
        'A'...'Z' => {},
        else => return false,
    }

    if (string.len == 1) return true;

    for (string) |char| switch (char) {
        'A'...'Z', '0'...'9', '_' => {},
        else => return false,
    };

    return true;
}

const Times = struct {
    val: []const u8,
    n: usize,

    const Self = @This();
    pub fn init(value: []const u8, n: usize) Self {
        return .{
            .val = value,
            .n = n,
        };
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (0..self.n) |_| {
            try writer.print("{s}", .{self.val});
        }
    }
};
