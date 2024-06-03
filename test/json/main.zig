const std = @import("std");
const json = @import("parser");
const testing = std.testing;
const talloc = testing.allocator;
const debug = @import("main_exec.zig").debug;

fn parse(input: [:0]const u8) !json.Json(.{}) {
    var p = try json.Json(.{}).init(talloc);
    const r = try p.parse(input);
    errdefer p.deinit();
    if (r != .pass) {
        debug(p, r);
        return error.TestUnexpectedResult;
    }
    try testing.expectEqual(p.chars.len, p.acc);
    return p;
}

test "null" {
    var p = try parse(" null \n");
    defer p.deinit();
    try testing.expect(p.value == .null);
}

test "true" {
    var p = try parse(" true \n");
    defer p.deinit();
    try testing.expect(p.value == .bool);
    try testing.expect(p.value.bool);
}

test "false" {
    var p = try parse(" false \n");
    defer p.deinit();
    try testing.expect(p.value == .bool);
    try testing.expect(!p.value.bool);
}

test "string" {
    const input = "\"abc\"";
    var p = try parse(input);
    defer p.deinit();
    try testing.expect(p.value == .string);
    try testing.expectEqualStrings(input, p.value.string);
}

test "string1" {
    const input =
        \\" !#[]\"\\\/\b\f\n\r\t"
    ;
    var p = try parse(input);
    defer p.deinit();
    try testing.expect(p.value == .string);
    try testing.expectEqualStrings(input, p.value.string);
}

test "number0" {
    var p = try parse("0");
    defer p.deinit();
    try testing.expect(p.value == .integer);
    try testing.expectEqual(0, p.value.integer);
}

test "number1" {
    var p = try parse("1.1e2");
    defer p.deinit();
    try testing.expect(p.value == .float);
    try testing.expectEqual(110, p.value.float);
}

test "array0" {
    var p = try parse("[ \n ]");
    defer p.deinit();
    try testing.expect(.array == p.value);
    try testing.expectEqual(0, p.value.array.items.len);
}

test "array1" {
    var p = try parse("[\"abc\", 0, null, true, false, {\"a\": 1} ]");
    defer p.deinit();
    try testing.expect(.array == p.value);
    defer {
        p.value.array.items[5].object.deinit();
        p.value.array.deinit();
    }
    try testing.expectEqual(6, p.value.array.items.len);
    try testing.expect(.string == p.value.array.items[0]);
    try testing.expectEqualStrings("\"abc\"", p.value.array.items[0].string);
    try testing.expect(.integer == p.value.array.items[1]);
    try testing.expectEqual(0, p.value.array.items[1].integer);
    try testing.expect(.null == p.value.array.items[2]);
    try testing.expect(.bool == p.value.array.items[3]);
    try testing.expect(p.value.array.items[3].bool);
    try testing.expect(.bool == p.value.array.items[4]);
    try testing.expect(!p.value.array.items[4].bool);
    try testing.expect(.object == p.value.array.items[5]);
    try testing.expectEqual(1, p.value.array.items[5].object.count());
    try testing.expectEqual(1, p.value.array.items[5].object.get("a").?.integer);
}

test "object0" {
    var p = try parse("{ }");
    defer p.deinit();
    try testing.expect(.object == p.value);
    try testing.expectEqual(0, p.value.object.count());
}

test "object2" {
    var p = try parse(
        \\{ "a": 1, "b": 2, "c": [3,4], "d": {"e": null} }
    );
    defer {
        p.value.object.getPtr("c").?.array.deinit();
        p.value.object.getPtr("d").?.object.deinit();
        p.value.object.deinit();
        p.deinit();
    }
    try testing.expect(.object == p.value);
    try testing.expectEqual(4, p.value.object.count());
    try testing.expectEqual(1, p.value.object.get("a").?.integer);
    try testing.expectEqual(2, p.value.object.get("b").?.integer);
    try testing.expectEqualSlices(
        std.json.Value,
        &.{ .{ .integer = 3 }, .{ .integer = 4 } },
        p.value.object.get("c").?.array.items,
    );
    try testing.expectEqual(.null, p.value.object.get("d").?.object.get("e"));
}
