const std = @import("std");
const json = @import("parser");
const testing = std.testing;
const talloc = testing.allocator;
const debug = @import("main_exec.zig").debug;

fn parse(input: [:0]const u8) !std.json.Value {
    var p = try json.Json(.{}).init(talloc);
    defer p.deinit();
    const r = try p.parse(input);
    return switch (r) {
        .pass => |value| value,
        else => {
            debug(p, r);
            return error.TestUnexpectedResult;
        },
    };
}

test "null" {
    const v = try parse(" null \n");
    try testing.expect(v == .null);
}

test "true" {
    const v = try parse(" true \n");
    try testing.expect(v == .bool);
    try testing.expect(v.bool);
}

test "false" {
    const v = try parse(" false \n");
    try testing.expect(v == .bool);
    try testing.expect(!v.bool);
}

test "string" {
    const input = "\"abc\"";
    const v = try parse(input);
    try testing.expect(v == .string);
    try testing.expectEqualStrings(input, v.string);
}

test "string1" {
    const input =
        \\" !#[]\"\\\/\b\f\n\r\t"
    ;
    const v = try parse(input);
    try testing.expect(v == .string);
    try testing.expectEqualStrings(input, v.string);
}

test "number0" {
    const v = try parse("0");
    try testing.expect(v == .integer);
    try testing.expectEqual(0, v.integer);
}

test "number1" {
    const v = try parse("1.1e2");
    try testing.expect(v == .float);
    try testing.expectEqual(110, v.float);
}

test "array0" {
    const v = try parse("[ \n ]");
    try testing.expect(.array == v);
    try testing.expectEqual(0, v.array.items.len);
}

test "array1" {
    var v = try parse("[\"abc\", 0, null, true, false, {\"a\": 1} ]");
    try testing.expect(.array == v);
    defer {
        v.array.items[5].object.deinit();
        v.array.deinit();
    }
    try testing.expectEqual(6, v.array.items.len);
    try testing.expect(.string == v.array.items[0]);
    try testing.expectEqualStrings("\"abc\"", v.array.items[0].string);
    try testing.expect(.integer == v.array.items[1]);
    try testing.expectEqual(0, v.array.items[1].integer);
    try testing.expect(.null == v.array.items[2]);
    try testing.expect(.bool == v.array.items[3]);
    try testing.expect(v.array.items[3].bool);
    try testing.expect(.bool == v.array.items[4]);
    try testing.expect(!v.array.items[4].bool);
    try testing.expect(.object == v.array.items[5]);
    try testing.expectEqual(1, v.array.items[5].object.count());
    try testing.expectEqual(1, v.array.items[5].object.get("a").?.integer);
}

test "object0" {
    const v = try parse("{ }");
    try testing.expect(.object == v);
    try testing.expectEqual(0, v.object.count());
}

test "object2" {
    var v = try parse(
        \\{ "a": 1, "b": 2, "c": [3,4], "d": {"e": null} }
    );
    defer {
        v.object.getPtr("c").?.array.deinit();
        v.object.getPtr("d").?.object.deinit();
        v.object.deinit();
    }
    try testing.expect(.object == v);
    try testing.expectEqual(4, v.object.count());
    try testing.expectEqual(1, v.object.get("a").?.integer);
    try testing.expectEqual(2, v.object.get("b").?.integer);
    try testing.expectEqualSlices(
        std.json.Value,
        &.{ .{ .integer = 3 }, .{ .integer = 4 } },
        v.object.get("c").?.array.items,
    );
    try testing.expectEqual(.null, v.object.get("d").?.object.get("e"));
}
