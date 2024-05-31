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
const TupleReturn = @import("parser").TupleReturn(.{});

test "Working" {
    const s = "1111111111111101010101010101";
    const allocator = std.testing.allocator;
    var parser = try TupleReturn.init(allocator);
    defer parser.deinit();

    const result = try parser.parse(s);
    try expect(switch (result) {
        .pass => |val| val == 21,
        else => false,
    });
}
