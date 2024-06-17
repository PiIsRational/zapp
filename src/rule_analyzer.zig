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
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ir = @import("low_ir.zig");

pub const AcceptanceSet = struct {
    values: [4]u64 = .{0} ** 4,

    const RangeIterator = struct {
        set: *const AcceptanceSet,
        index: u8,

        pub fn next(self: *RangeIterator) ?ir.Range {
            if (self.index == 255) return null;

            while (self.index < 255 and
                !self.set.matchesChar(self.index)) : (self.index += 1)
            {}

            const from = if (self.index == 255 and !self.set.matchesChar(self.index))
                return null
            else
                self.index;

            while (self.index < 255 and
                self.set.matchesChar(self.index)) : (self.index += 1)
            {}

            const to = if (self.index == 255 and self.set.matchesChar(self.index))
                255
            else
                self.index - 1;

            return .{
                .from = from,
                .to = to,
            };
        }
    };

    pub fn rangeIter(self: *const AcceptanceSet) RangeIterator {
        return .{
            .set = self,
            .index = 0,
        };
    }

    /// merges two acceptance sets
    pub fn merge(self: *AcceptanceSet, other: *AcceptanceSet) void {
        for (&self.values, &other.values) |*main_val, incoming_val| {
            main_val.* |= incoming_val;
        }
    }

    pub fn invert(self: *AcceptanceSet) void {
        for (&self.values) |*value| value.* = ~value.*;
    }

    pub fn cut(self: *AcceptanceSet, other: *const AcceptanceSet) void {
        for (&self.values, &other.values) |*main_val, incoming_val| {
            main_val.* &= ~incoming_val;
        }
    }

    pub fn disjoint(self: *const AcceptanceSet, other: *const AcceptanceSet) bool {
        var clone = self.*;
        clone.intersect(other);
        return clone.isEmpty();
    }

    pub fn intersect(self: *AcceptanceSet, other: *const AcceptanceSet) void {
        for (&self.values, &other.values) |*main_val, incoming_val| {
            main_val.* &= incoming_val;
        }
    }

    /// inclusive range
    pub fn addRange(self: *AcceptanceSet, from: u8, to: u8) void {
        for (from..to + 1) |char| self.addChar(@intCast(char));
    }

    pub fn addChar(self: *AcceptanceSet, char: u8) void {
        self.values[char >> 6] |= @as(u64, 1) << @as(u6, @intCast(char & 0x3F));
    }

    pub fn matchesChar(self: *const AcceptanceSet, char: u8) bool {
        return self.values[char >> 6] & (@as(u64, 1) << @as(u6, @intCast(char & 0x3F))) != 0;
    }

    /// inclusive range
    pub fn matchesRange(self: *const AcceptanceSet, from: u8, to: u8) bool {
        for (from..to + 1) |char| if (!self.matchesChar(char)) return false;
        return true;
    }

    pub fn isFull(self: *const AcceptanceSet) bool {
        for (0..4) |i| if (self.values[i] != std.math.maxInt(u64)) return false;
        return true;
    }

    pub fn isEmpty(self: *const AcceptanceSet) bool {
        for (0..4) |i| if (self.values[i] != 0) return false;
        return true;
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        var iter = self.rangeIter();
        while (iter.next()) |range| {
            try writer.print("{s}, ", .{range});
        }
    }
};
