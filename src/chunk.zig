const std = @import("std");

const Value = @import("value.zig").Value;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

/// The instruction set for the VM
pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_DEFINE_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_RETURN,
    _,

    pub fn byte(op: OpCode) u8 {
        return @intFromEnum(op);
    }
};

/// A single array of instructions
pub const Chunk = struct {
    code: ArrayList(OpCode), // Raw instructions (bytecode)
    lines: ArrayList(usize), // Line numbers
    constants: ArrayList(Value), // Constant values
    alloc: Allocator,

    pub fn init(alloc: Allocator) Chunk {
        return Chunk{
            .alloc = alloc,
            .code = ArrayList(OpCode).init(alloc),
            .lines = ArrayList(usize).init(alloc),
            .constants = ArrayList(Value).init(alloc),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        for (self.constants.items) |*value| {
            value.deinit(self.alloc);
        }
        self.constants.deinit();
        self.* = undefined;
    }

    pub fn writeChunk(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(@enumFromInt(byte));
        try self.lines.append(line);
    }

    pub fn addConstant(self: *Chunk, value: Value) u8 {
        self.constants.append(value) catch return 0;
        std.debug.assert(self.constants.items.len <= 256);
        return @intCast(self.constants.items.len - 1);
    }
};
