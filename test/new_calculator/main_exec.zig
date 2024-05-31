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
const parser = @import("new_calc.zig");

const MaxSize = 1024 * 1024;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const stdin = std.io.getStdIn().reader();
    const allocator = gpa.allocator();
    var buffer = [_]u8{0} ** (MaxSize + 1);
    var err = false;
    var calc = try parser.NewCalc(.{}).init(allocator);
    defer calc.deinit();
    std.debug.print("calculator\n", .{});
    while (true) {
        const len = try stdin.read(buffer[0..]);
        if (len == buffer.len) {
            err = true;
            std.debug.print(
                "error: the input length should be at most {d}\n",
                .{len},
            );
        }
        if (err and len != buffer.len) {
            err = false;
            continue;
        }

        buffer[len] = 0;
        const str = buffer[0..len :0];
        std.debug.print("got string: {s}", .{str});
        const result = try calc.parse(str);
        switch (result) {
            .pass => |res| std.debug.print("got {d}\n", .{res}),
            else => std.debug.print("it failed :(\n", .{}),
        }
    }
}
