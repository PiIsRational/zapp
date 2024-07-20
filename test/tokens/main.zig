const std = @import("std");
const expect = std.testing.expect;
const Token = @import("parser").Token(.{});

test "correct match" {
    const to_parse =
        \\// Hello World
        \\/// Ola
    ;
    const expected =
        \\// Hello World
        \\
    ;

    const allocator = std.testing.allocator;
    var p = try Token.init(allocator);
    defer p.deinit();
    const result = try p.parse(to_parse);
    try expect(switch (result) {
        .pass => |val| blk: {
            defer allocator.free(val);

            try expect(val.len == 2);
            const it = val[1];
            try expect(it.skip);
            break :blk std.mem.eql(u8, it.match, expected);
        },
        else => false,
    });
}

test "no slash match" {
    const allocator = std.testing.allocator;
    var p = try Token.init(allocator);
    defer p.deinit();
    const result = try p.parse("/");
    try expect(switch (result) {
        .parse_fail => true,
        .pass => |val| blk: {
            for (val) |v| std.debug.print("\"{s}\"\n", .{v.match});
            allocator.free(val);
            break :blk false;
        },
        else => false,
    });
}
