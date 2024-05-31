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
const ir = @import("peg_ir.zig");
const PegIr = ir.PegIr;
const Passes = @import("peg_check.zig");
const assert = std.debug.assert;
const Error = @import("errors.zig");
const Parser = @import("peg_parser.zig").Zapp(.{});
const graphGen = @import("graphviz_gen.zig").generate;
const lower = @import("lowering.zig").lower;
const CodeGen = @import("new_gen.zig");
const config = @import("config");

pub const MaxInputSize = 1024 * 1024 * 1024;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leak = gpa.deinit();
        assert(leak == .ok);
    }
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    try parseArgs(allocator, args);
}

fn parseArgs(allocator: Allocator, args: [][]const u8) !void {
    const stdout = std.io.getStdOut().writer();
    if (args.len < 2) {
        try Error.print("no command was specified", null, "", &.{});
        try sendHelp(true);
        std.process.exit(1);
    }

    const command = args[1];
    if (std.mem.eql(u8, command, "gen")) {
        if (args.len < 4) {
            try Error.print("expected: zapp gen <source> <dst>", null, "", &.{});
            std.process.exit(1);
        }

        const name_opt = Option([]const u8, "name", "");
        const verbose_ir_opt = Option(bool, "verbose-ir", false);
        const inline_opt = Option(bool, "inline", true);

        const src = args[2];
        const dst = args[3];
        const name = name_opt.parse(args[4..]) catch "";
        const verbose_ir = verbose_ir_opt.parse(args[4..]) catch false;
        const @"inline" = inline_opt.parse(args[4..]) catch true;

        try parserGen(
            src,
            dst,
            allocator,
            verbose_ir,
            if (name.len == 0) null else name,
            @"inline",
        );
    } else if (std.mem.eql(u8, command, "help")) {
        try sendHelp(false);
    } else if (std.mem.eql(u8, command, "version")) {
        try stdout.print("zapp {}\n", .{config.Version});
    } else {
        try Error.print("the given command is not supported", null, "", &.{});
        try sendHelp(true);
        std.process.exit(1);
    }
}

fn sendHelp(err: bool) !void {
    const stderr = std.io.getStdErr().writer();
    const stdout = std.io.getStdOut().writer();
    const writer = if (err) stderr else stdout;
    const HelpString =
        \\
        \\the commands supported by this zapp version are:
        \\
        \\  gen <source> <dest>   generates a new parser takes in the source
        \\                        file and destination file paths
        \\  version               prints the version of zapp
        \\  help                  prints this help message
        \\
        \\the options are:
        \\
        \\  --name=[string]      sets the name to the genrated parser 
        \\                       (default: the grammar name)
        \\  --verbose-ir=[bool]  prints the peg ir of the source after
        \\                       transformations (default: false)
        \\  --inline=[bool]      uses inlining for some rules (default: true)
        \\
    ;

    try writer.print("{s}\n", .{HelpString});
}

/// the main function for the parser generation
fn parserGen(
    src_path: []const u8,
    dst_path: []const u8,
    allocator: Allocator,
    verbose_pir: bool,
    parser_name: ?[]const u8,
    inlining: bool,
) !void {
    const stdout = std.io.getStdOut().writer();

    // read the full source
    const src_file = std.fs.cwd().openFile(src_path, .{}) catch {
        try Error.print("could not open the given source file", null, "", &.{});
        return;
    };

    const src = src_file.readToEndAllocOptions(
        allocator,
        MaxInputSize,
        null,
        1,
        0,
    ) catch |err| {
        switch (err) {
            error.FileTooBig => {
                try Error.print(
                    "the input file is too large (should be atmost 1GB)",
                    null,
                    "",
                    &.{},
                );
            },
            else => return err,
        }
        return;
    };
    defer allocator.free(src);

    // parse
    var parser = try Parser.init(allocator);
    defer parser.deinit();

    parser.ir = PegIr.init(allocator);
    parser.ir.chars = src;
    parser.ir.file_name = src_path;

    switch (try parser.parse(src)) {
        .pass => {},
        .parse_fail => |err| {
            try Error.parseError(
                allocator,
                src_path,
                src,
                err.last_found,
                err.expected,
            );
            return;
        },
        .infer_fail => |err| {
            try Error.print(err.msg, null, "", &.{});
            return;
        },
    }

    var p_ir = parser.ir;
    defer p_ir.deinit();
    if (parser_name) |name| {
        p_ir.name = name;
    }

    // optimize
    var passes: Passes = undefined;
    if (!try passes.optimize(&p_ir, inlining)) return;

    if (verbose_pir) {
        try stdout.print("{s}\n", .{p_ir});
    }

    // lower
    var lir = try lower(allocator, p_ir);
    defer lir.deinit();

    // generate
    const f = std.fs.cwd().createFile(dst_path, .{}) catch {
        try Error.print("could not create the destination file", null, "", &.{});
        return;
    };
    var ngen: CodeGen = undefined;
    try ngen.generate(lir, f.writer());
}

fn Option(comptime T: type, comptime name: []const u8, comptime default: T) type {
    return struct {
        const OptionParseError = error{
            BadBool,
        };

        pub fn parse(input: []const []const u8) OptionParseError!T {
            for (input) |option| {
                if (option.len <= 4 + name.len) continue;

                if (option[0] != '-' or option[1] != '-' or option[2 + name.len] != '=' or
                    !std.mem.eql(u8, option[2 .. name.len + 2], name))
                {
                    continue;
                }

                const string = option[name.len + 3 ..];
                return switch (T) {
                    []const u8 => parseString(string, option),
                    bool => parseBool(string, option),
                    else => @compileError("option with unsupported type!"),
                };
            }

            return default;
        }

        fn parseString(input: []const u8, _: []const u8) []const u8 {
            return input;
        }

        fn parseBool(input: []const u8, full_string: []const u8) OptionParseError!bool {
            if (std.mem.eql(u8, input, "true")) return true;
            if (std.mem.eql(u8, input, "false")) return false;

            Error.print(
                "bad boolean",
                null,
                full_string,
                &.{.{
                    .place = input,
                    .msg = "not a boolean",
                    .t = .ERROR,
                }},
            ) catch {};
            return error.BadBool;
        }
    };
}
