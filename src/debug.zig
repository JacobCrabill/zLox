const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

pub const log = std.log.scoped(.zlox);

/// Print the disassembled instruction to the console
/// Disabled in non-Debug builds
pub fn disassembleChunk(chunk: *const Chunk, name: []const u8) void {
    if (builtin.mode == .Debug) {
        std.debug.print("== {s} ==\n", .{name});

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
        .OP_ADD, .OP_SUBTRACT, .OP_MULTIPLY, .OP_DIVIDE => return simpleInstruction(op, offset),
        .OP_NEGATE, .OP_RETURN => return simpleInstruction(op, offset),
        else => {
            log.info("Unknown opcode {}", .{op});
            return offset + 1;
        },
    }
}

pub fn constantInstruction(op: OpCode, chunk: *const Chunk, offset: usize) usize {
    const constant: u8 = @intFromEnum(chunk.code.items[offset + 1]);
    const value: f64 = chunk.constants.items[constant].value;
    std.debug.print("{s:<16} {d:4} '{e}'\n", .{ @tagName(op), constant, value });
    return offset + 2;
}

pub fn simpleInstruction(op: OpCode, offset: usize) usize {
    std.debug.print("{s:<16}\n", .{@tagName(op)});
    return offset + 1;
}

pub fn printValue(value: Value) void {
    std.debug.print("{e}", .{value.value});
}
