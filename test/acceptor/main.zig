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
const Json = @import("parser").JsonParser(.{});

test "it accepts" {
    const allocator = std.testing.allocator;
    const data =
        \\ { "hello" : "world" }
    ;
    var parser = try Json.init(allocator);
    defer parser.deinit();
    const result = try parser.parse(data);
    try expect(switch (result) {
        .pass => true,
        else => false,
    });
}
