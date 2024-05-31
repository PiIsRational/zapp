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
const Allocator = std.mem.Allocator;

const su = @import("string_utils.zig");

// control
const Reset = "\x1b[0m";
const Bold = "\x1b[1m";
const Dim = "\x1b[2m";

// colors
const Red = "\x1b[31;1m";
const White = "\x1b[37;1m";
const Cyan = "\x1b[36;1m";
const Green = "\x1b[32;1m";
const Blue = "\x1b[34m";
const Yellow = "\x1b[0;33m";

pub const Error = struct {
    place: []const u8,
    msg: []const u8,
    t: ErrorType,
};

pub const ErrorType = enum(u8) {
    ERROR,
    WARNING,
    INFO,
    HELP,
};

const Colors = std.EnumArray(ErrorType, []const u8).init(.{
    .ERROR = Red,
    .WARNING = Yellow,
    .INFO = Cyan,
    .HELP = Green,
});

const Names = std.EnumArray(ErrorType, []const u8).init(.{
    .ERROR = "error",
    .WARNING = "warning",
    .INFO = "info",
    .HELP = "help",
});

pub const MaxArgs = 10;
pub const PrintError = error{
    TOO_MUCH_ARGS,
    BAD_ARG,
};

pub fn parseError(
    allocator: Allocator,
    path: ?[]const u8,
    src: []const u8,
    last_matched: []const u8,
    expected_after: []const u8,
) !void {
    const message = try std.fmt.allocPrint(
        allocator,
        "expected '{s}'",
        .{su.toLower(expected_after)},
    );
    defer allocator.free(message);

    try print(
        "unexpected character sequence",
        path,
        src,
        if (expected_after.len > 0)
            &.{ .{
                .place = last_matched,
                .msg = "last recognized sequence",
                .t = .ERROR,
            }, .{
                .place = last_matched,
                .msg = message,
                .t = .HELP,
            } }
        else
            &.{.{
                .place = last_matched,
                .msg = "last recognized sequence",
                .t = .ERROR,
            }},
    );
}

/// sends a message to stderr
/// `message`: the main error message
/// `file`: the path to the file in which the error occured
/// `base`: the text representation of the file
/// `args`: the additional error messages
///
/// the function shows one main place (the first arg)
pub fn print(
    message: []const u8,
    file: ?[]const u8,
    base: []const u8,
    args: []const Error,
) !void {
    if (args.len > MaxArgs) {
        return PrintError.TOO_MUCH_ARGS;
    }

    const stderr = std.io.getStdErr().writer();
    if (args.len == 0) {
        try sendError(
            message,
            Names.get(.ERROR),
            Colors.get(.ERROR),
        );
        try stderr.print("\n", .{});
        return;
    }

    try sendError(
        message,
        Names.get(args[0].t),
        Colors.get(args[0].t),
    );

    const place = args[0].place;
    const first_line = getFirstLineOf(base, place) orelse {
        try stderr.print("\n", .{});
        return;
    };

    if (file) |file_name| {
        try stderr.print(Dim ++ " [{s}:{d}:{d}]\n" ++ Reset, .{
            file_name,
            first_line.line.lineno,
            first_line.content_start + 1,
        });
    } else {
        try stderr.print("\n", .{});
    }

    try stderr.print(Dim ++ "     |\n" ++ Reset, .{});

    var lineData: [MaxArgs]LineContent = undefined;
    var conversions: [MaxArgs]usize = undefined;
    const lines = lineData[0..args.len];
    const convs = conversions[0..args.len];

    for (args, 0..) |arg, i| {
        convs[i] = i;
        lines[i] = getFirstLineOf(base, arg.place) orelse
            return error.BAD_ARGS;
    }

    sortByLine(convs, lines);

    var last_lineno: usize = lines[0].line.lineno;
    for (lines, 0..) |line, i| {
        const arg = args[convs[i]];
        const multiple_lines = i + 1 < lines.len and
            last_lineno != lines[i + 1].line.lineno and
            last_lineno + 1 != lines[i + 1].line.lineno;

        if (arg.t == .HELP) {
            try sendSimpleMessage(
                Colors.get(arg.t),
                Names.get(arg.t),
                arg.msg,
            );
        } else {
            try sendLineMessage(
                Colors.get(arg.t),
                arg.msg,
                line,
            );
        }

        if (multiple_lines) {
            try stderr.print(Dim ++ "    ...\n" ++ Reset, .{});
        }

        last_lineno = line.line.lineno;
    }

    try stderr.print("\n", .{});
}

fn sendSimpleMessage(
    color: []const u8,
    base: []const u8,
    msg: []const u8,
) !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print("{s}{s}:" ++ Reset ++ " {s}\n", .{ color, base, msg });
}

fn sendLineMessage(
    color: []const u8,
    message: []const u8,
    line: LineContent,
) !void {
    const stderr = std.io.getStdErr().writer();

    try stderr.print("{d:>4} " ++ Dim ++ "|" ++ Reset ++ " {s}\n", .{
        line.line.lineno,
        line.line.line,
    });
    try stderr.print(Dim ++ "     | ", .{});

    for (0..line.content_start) |_| {
        try stderr.print(" ", .{});
    }

    try stderr.print(Reset ++ "{s}", .{color});
    for (0..line.content.len) |_| {
        try stderr.print("^", .{});
    }

    try stderr.print(" {s}\n" ++ Reset, .{message});
}

fn sendError(
    message: []const u8,
    case: []const u8,
    color: []const u8,
) !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print(
        "{s}{s}:" ++ Reset ++ Bold ++ " {s}" ++ Reset,
        .{ color, case, message },
    );
}

fn sortByLine(convs: []usize, lines: []LineContent) void {
    var iter: isize = @as(isize, @intCast(lines.len)) - 1;
    while (iter >= 0) : (iter -= 1) {
        var j: isize = 0;
        while (j < iter) : (j += 1) {
            if (lines[@intCast(iter)].line.lineno <
                lines[@intCast(j)].line.lineno)
            {
                std.mem.swap(
                    LineContent,
                    &lines[@intCast(iter)],
                    &lines[@intCast(j)],
                );
                std.mem.swap(
                    usize,
                    &convs[@intCast(iter)],
                    &convs[@intCast(j)],
                );
            }
        }
    }

    var conversionsInv: [MaxArgs]usize = undefined;
    for (convs, 0..) |conv, i| {
        conversionsInv[conv] = i;
    }

    for (convs, 0..) |*conv, i| {
        conv.* = conversionsInv[i];
    }
}

fn getFirstLineOf(base: []const u8, place: []const u8) ?LineContent {
    // check that place does not contain newlines
    for (place, 0..) |char, i| {
        if (char != '\n' and char != '\r') continue;

        return getLineOf(base, place[0..i]);
    }

    return getLineOf(base, place);
}

fn getLineOf(
    base: []const u8,
    place: []const u8,
) ?LineContent {
    // check that place is somewhere in base
    if (@intFromPtr(place.ptr) < @intFromPtr(base.ptr) or
        @intFromPtr(place.ptr + place.len) >
        @intFromPtr(base.ptr + base.len))
    {
        return null;
    }

    // check that place does not contain newlines
    for (place) |char| {
        if (char == '\n' or char == '\r') {
            return null;
        }
    }

    var lines: usize = 1;
    var line_start: usize = 0;
    var i: usize = 0;
    while (base.ptr + i != place.ptr) : (i += 1) {
        switch (base[i]) {
            '\n' => {
                lines += 1;
                line_start = i + 1;
            },
            '\r' => {
                lines += 1;

                if (base[i + 1] == '\n') {
                    i += 1;
                }

                line_start = i + 1;
            },
            else => {},
        }
    }
    const content_start = i - line_start;

    // get the line end
    i += place.len;
    var line_end: usize = 0;
    while (i < base.len) : (i += 1) {
        switch (base[i]) {
            '\n', '\r' => {
                line_end = i;
                break;
            },
            else => {},
        }
    }

    if (i == base.len) {
        line_end = base.len;
    }

    return .{
        .line = .{
            .line = base[line_start..line_end],
            .lineno = lines,
        },
        .content = place,
        .content_start = content_start,
    };
}

const LineContent = struct {
    line: Line,
    content: []const u8,
    content_start: usize,
};

const Line = struct {
    line: []const u8,
    lineno: usize,
};
