const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Object = @import("value.zig").Object;

pub const log = std.log.scoped(.zlox);

/// Print the disassembled instruction to the console
/// Disabled in non-Debug builds
pub fn disassembleChunk(chunk: *const Chunk, name: []const u8) void {
    if (builtin.mode == .Debug) {
        std.debug.print("== {s} ==\n", .{name});
        std.debug.print("Idx  Line Op\n", .{});

        var offset: usize = 0;
        while (offset < chunk.code.items.len) {
            offset = disassembleInstruction(chunk, offset);
        }
    }
}

pub fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else if (chunk.lines.items.len > 0) {
        std.debug.print("{d:>4} ", .{chunk.lines.items[offset]});
    }

    std.debug.assert(chunk.code.items.len > 0);

    const op: OpCode = chunk.code.items[offset];
    switch (op) {
        .OP_CONSTANT => return constantInstruction(op, chunk, offset),
        .OP_DEFINE_GLOBAL => return constantInstruction(op, chunk, offset),
        else => return simpleInstruction(op, offset),
    }
}

pub fn constantInstruction(op: OpCode, chunk: *const Chunk, offset: usize) usize {
    const constant: u8 = @intFromEnum(chunk.code.items[offset + 1]);
    const value: Value = chunk.constants.items[constant];

    std.debug.print("{s:<16} {d:4} ", .{ @tagName(op), constant });
    printValue(value);
    std.debug.print("\n", .{});

    return offset + 2;
}

pub fn simpleInstruction(op: OpCode, offset: usize) usize {
    std.debug.print("{s:<16}\n", .{@tagName(op)});
    return offset + 1;
}

pub fn printObject(obj: Object) void {
    switch (obj) {
        .string => |s| std.debug.print("Obj.String: '{s}'", .{s}),
    }
}

pub fn printValue(value: Value) void {
    switch (value) {
        .number => |num| std.debug.print("'{e}'", .{num}),
        .bool => |b| std.debug.print("'{}'", .{b}),
        .none => std.debug.print("'none'", .{}),
        .object => |obj| printObject(obj.*),
    }
}
