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
const parser = @import("parser");

test "add no memo" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{ .use_memo = false }).init(allocator);
    defer calc.deinit();

    const add = try calc.parse("1 + 2 + -3 + 30");
    try expect(switch (add) {
        .pass => |val| val == 30,
        else => false,
    });
    try expect(calc.stats().memo == 0);
}

test "sub no memo" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{ .use_memo = false }).init(allocator);
    defer calc.deinit();

    const sub = try calc.parse("42 - 3 - 13 - -4");
    try expect(switch (sub) {
        .pass => |val| val == 30,
        else => false,
    });
    try expect(calc.stats().memo == 0);
}

test "mul no memo" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{ .use_memo = false }).init(allocator);
    defer calc.deinit();

    const mul = try calc.parse("-1 * 3 * -2 * 5");
    try expect(switch (mul) {
        .pass => |val| val == 30,
        else => false,
    });
    try expect(calc.stats().memo == 0);
}

test "div no memo" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{ .use_memo = false }).init(allocator);
    defer calc.deinit();

    const div = try calc.parse("2 * 360 / 2 / 3 / 4");
    try expect(switch (div) {
        .pass => |val| val == 30,
        else => false,
    });
    try expect(calc.stats().memo == 0);
}

test "full full limit" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{
        .stack_limit = 0,
    }).init(allocator);
    defer calc.deinit();

    // expected to fail because of the stack limit
    _ = calc.parse("(3 * 4 + 2) / 3 + 5 - -12 + -13 / 5") catch return;
    try expect(false);
}

test "full no memo" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{ .use_memo = false }).init(allocator);
    defer calc.deinit();

    const result = try calc.parse("(3 * 4 + 2) / 3 + 5 - -12 + -13 / 5");
    try expect(switch (result) {
        .pass => |val| val == 18,
        else => false,
    });
    try expect(calc.stats().memo == 0);
}

test "add" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{}).init(allocator);
    defer calc.deinit();

    const add = try calc.parse("1 + 2 + -3 + 30");
    try expect(switch (add) {
        .pass => |val| val == 30,
        else => false,
    });
}

test "sub" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{}).init(allocator);
    defer calc.deinit();

    const sub = try calc.parse("42 - 3 - 13 - -4");
    try expect(switch (sub) {
        .pass => |val| val == 30,
        else => false,
    });
}

test "mul" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{}).init(allocator);
    defer calc.deinit();

    const mul = try calc.parse("-1 * 3 * -2 * 5");
    try expect(switch (mul) {
        .pass => |val| val == 30,
        else => false,
    });
}

test "div" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{}).init(allocator);
    defer calc.deinit();

    const div = try calc.parse("2 * 360 / 2 / 3 / 4");
    try expect(switch (div) {
        .pass => |val| val == 30,
        else => false,
    });
}

test "full" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var calc = try parser.NewCalc(.{}).init(allocator);
    defer calc.deinit();

    const result = try calc.parse("(3 * 4 + 2) / 3 + 5 - -12 + -13 / 5");
    try expect(switch (result) {
        .pass => |val| val == 18,
        else => false,
    });
}
