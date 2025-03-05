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
const Error = @import("errors.zig");
const Parser = @import("parser").Parser(.{ .use_memo = true });
const assert = std.debug.assert;

const Green = "\x1b[32;1m";
const Red = "\x1b[31;1m";
const Blue = "\x1b[34m";
const Reset = "\x1b[0m";

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
    const stdout = std.io.getStdOut().writer();

    try stdout.print("\n", .{});
    if (args.len != 3) {
        try stdout.print("error expected path to parse\n", .{});
    }

    const show_memo = args[2][0] == 't';

    var parser = try Parser.init(allocator);
    defer parser.deinit();

    const dir = try std.fs.cwd().openDir(args[1], .{ .iterate = true });

    var files_total: usize = 0;
    var passed_files: usize = 0;
    var failed_files: usize = 0;
    var total_size: usize = 0;

    var walker = try dir.walk(allocator);
    defer walker.deinit();

    const start_ms = std.time.milliTimestamp();
    while (try walker.next()) |entry| {
        switch (entry.kind) {
            .file => files_total += 1,
            else => continue,
        }

        const src_file = try entry.dir.openFile(entry.basename, .{});
        const src = try src_file.readToEndAllocOptions(allocator, MaxInputSize, null, 1, 0);
        defer allocator.free(src);
        total_size += src.len;

        switch (try parser.parse(src)) {
            .pass => {
                passed_files += 1;
                try stdout.print(Green ++ "pass:" ++ Reset ++ " {s}\n", .{entry.path});
                const stats = parser.stats();
                if (show_memo) {
                    try stdout.print(
                        "max memo: {d}\tstate: {d}\tmemo: {d}\n",
                        .{ stats.max_memo, stats.max_memo_state, stats.memo },
                    );
                }
            },
            .parse_fail => |err| {
                failed_files += 1;
                try stdout.print(Red ++ "fail:" ++ Reset ++ " {s}\n", .{entry.path});
                try Error.parseError(
                    allocator,
                    entry.path,
                    src,
                    err.last_found,
                    err.expected,
                );
            },
            // there is no infer in those grammars
            else => unreachable,
        }
    }
    const ellapsed_millis = std.time.milliTimestamp() - start_ms;

    try stdout.print(Blue ++ "\n     Stats:     \n================\n" ++ Reset, .{});
    try stdout.print(
        "\n" ++ Green ++ "passed" ++ Reset ++ " {d}/{d} files\n",
        .{ passed_files, files_total },
    );
    try stdout.print(
        Red ++ "failed" ++ Reset ++ " {d}/{d} files\n\n",
        .{ failed_files, files_total },
    );

    try stdout.print(Blue ++ "time stats:\n" ++ Reset, .{});
    try stdout.print("elapsed time: {d} ms\n", .{ellapsed_millis});
    try stdout.print("parsed bytes: {d}\n", .{total_size});
    try stdout.print(
        Blue ++ "  ==> " ++ Reset ++ "approx. speed: {d}/s\n\n",
        .{std.fmt.fmtIntSizeBin(@intCast(@divFloor(1000 * total_size, @as(usize, @intCast(ellapsed_millis)))))},
    );

    try stdout.print(Blue ++ "memory stats:" ++ Reset ++ "\n{s}\n", .{parser.stats()});
}
