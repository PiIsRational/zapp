//!
//! zig build && zig build-exe -femit-bin=test/json/parse --dep parser -Mroot=test/json/main_exec.zig -Mparser=zig-out/gen/json.zig
//!
//! zig build && zig run --dep parser -Mroot=test/json/main_exec.zig -Mparser=zig-out/gen/json.zig -- test/json/test.json
//!

const std = @import("std");
const json = @import("parser");

pub fn debug(p: json.Json(.{}), r: json.ParseReturn) void {
    if (r == .parse_fail) {
        const pos = @intFromPtr(r.parse_fail.last_found.ptr) -| @intFromPtr(p.chars.ptr);
        const len = @min(20, p.chars.len -| pos);
        std.log.err(
            "parse failure: expected '{s}' at position {}. found '{s}'",
            .{ r.parse_fail.expected, pos, p.chars[pos..][0..len] },
        );
    } else if (r == .infer_fail) {
        std.log.err("infer failure: {?} {s}", .{ r.infer_fail.err, r.infer_fail.msg });
    }
}

pub fn main() !void {
    var arena_state = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena_state.deinit();
    const arena = arena_state.allocator();
    var p = try json.Json(.{}).init(arena);
    const args = try std.process.argsAlloc(arena);
    if (args.len < 2) return error.MissingFileArg;
    std.log.info("parsing {s}", .{args[1]});
    const f = try std.fs.cwd().openFile(args[1], .{});
    defer f.close();
    const input = try f.readToEndAllocOptions(arena, 1024 * 1024 * 20, null, 1, 0);
    const r = try p.parse(input);

    if (r != .pass) {
        debug(p, r);
        return error.ParseFailure;
    }
    if (p.chars.len != p.acc) {
        const len = @min(20, p.chars.len -| p.acc);
        std.log.err("parse failure: failed to consume all input. stopped at position {}: '{s}'\n", .{ p.acc, p.chars[0..len] });
        return error.ParseFailure;
    }
    std.log.info("parse successful", .{});

    // try std.json.stringify(p.value, .{}, std.io.getStdOut().writer());
}
