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
const QuestionReturn = @import("parser").QuestionReturn(.{});

test "Returns null" {
    const without = "0";
    const allocator = std.testing.allocator;
    var parser = try QuestionReturn.init(allocator);
    defer parser.deinit();
    const result = try parser.parse(without);
    try expect(switch (result) {
        .pass => |val| val == 0,
        else => false,
    });
}

test "Returns Value" {
    const with = "10";
    const allocator = std.testing.allocator;
    var parser = try QuestionReturn.init(allocator);
    defer parser.deinit();
    const result = try parser.parse(with);
    try expect(switch (result) {
        .pass => |val| val == 1,
        else => false,
    });
}
