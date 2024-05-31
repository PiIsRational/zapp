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
const expect = std.testing.expect;
const Plus = @import("parser").Plus(.{});

test "Get right list" {
    const to_parse = "10 20\t40  42     73 ";
    const to_compare = [_]usize{ 73, 42, 40, 20, 10 };

    const allocator = std.testing.allocator;
    var p = try Plus.init(allocator);
    defer p.deinit();

    const result = try p.parse(to_parse);
    try expect(switch (result) {
        .pass => |val| blk: {
            defer val.deinit();
            break :blk std.mem.eql(usize, &to_compare, val.items);
        },
        else => false,
    });
}

test "fail empty" {
    const to_parse = "";
    const allocator = std.testing.allocator;
    var p = try Plus.init(allocator);
    defer p.deinit();

    const result = try p.parse(to_parse);
    try expect(switch (result) {
        .parse_fail => true,
        else => false,
    });
}
