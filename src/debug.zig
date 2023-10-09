const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub const log = std.log.scoped(.zlox);

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:>4} ", .{chunk.lines.items[offset]});
    }

    const op: OpCode = chunk.code.items[offset];
    switch (op) {
        .OP_CONSTANT => return constantInstruction("OP_CONSTANT", chunk, offset),
        .OP_RETURN => return simpleInstruction("OP_RETURN", offset),
        else => {
            log.info("Unknown opcode {}", .{op});
            return offset + 1;
        },
    }
}

pub fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant: u8 = @intFromEnum(chunk.code.items[offset + 1]);
    const value: f64 = chunk.constants.items[constant].value;
    std.debug.print("{s:<16} {d:4} '{e}'\n", .{ name, constant, value });
    return offset + 2;
}

pub fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s:<16}\n", .{name});
    return offset + 1;
}
