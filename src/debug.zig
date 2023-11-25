const std = @import("std");
const builtin = @import("builtin");

const zlox = struct {
    usingnamespace @import("chunk.zig");
    usingnamespace @import("value.zig");
};

const Chunk = zlox.Chunk;
const OpCode = zlox.OpCode;
const Value = zlox.Value;
const Object = zlox.Object;
const Function = zlox.Function;

pub const log = std.log.scoped(.zlox);

pub const Verbosity = enum(u8) {
    Silent,
    Normal,
    Verbose,
};

/// Enable/Disable extra verbosity in output
pub const verbosity: Verbosity = if (builtin.mode == .Debug) .Normal else .Silent;

/// Print the disassembled instruction to the console
/// Disabled in non-Debug builds
pub fn disassembleChunk(chunk: *const Chunk, name: []const u8) void {
    if (verbosity != .Silent) {
        std.debug.print("== {s} ==\n", .{name});
        std.debug.print("{s:<4} {s:<4} {s:<16} Data\n", .{ "ip", "Line", "OpCode" });

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
    } else if (offset < chunk.lines.items.len) {
        std.debug.print("{d:>4} ", .{chunk.lines.items[offset]});
    }

    std.debug.assert(chunk.code.items.len > 0);

    const op: OpCode = chunk.code.items[offset];
    switch (op) {
        .OP_CONSTANT => return constantInstruction(op, chunk, offset),
        .OP_DEFINE_GLOBAL => return constantInstruction(op, chunk, offset),
        .OP_GET_GLOBAL, .OP_SET_GLOBAL => return constantInstruction(op, chunk, offset),
        .OP_GET_LOCAL, .OP_SET_LOCAL => return byteInstruction(op, chunk, offset),
        .OP_GET_UPVALUE, .OP_SET_UPVALUE => return byteInstruction(op, chunk, offset),
        .OP_JUMP_IF_FALSE, .OP_JUMP => return jumpInstruction(op, 1, chunk, offset),
        .OP_LOOP => return jumpInstruction(op, -1, chunk, offset),
        .OP_CALL => return byteInstruction(op, chunk, offset),
        .OP_CLOSURE => return closureInstruction(op, chunk, offset),
        else => return simpleInstruction(op, offset),
    }
}

fn constantInstruction(op: OpCode, chunk: *const Chunk, offset: usize) usize {
    const constant: u8 = chunk.code.items[offset + 1].byte();
    const value: Value = chunk.constants.items[constant];

    std.debug.print("{s:<16} {d:>4} ", .{ @tagName(op), constant });
    printValue(value);
    std.debug.print("\n", .{});

    return offset + 2;
}

fn closureInstruction(op: OpCode, chunk: *const Chunk, in_offset: usize) usize {
    var offset: usize = in_offset + 1;
    const constant: u8 = chunk.code.items[offset].byte();
    const value: Value = chunk.constants.items[constant];
    offset += 1;

    // Print the function name
    std.debug.print("{s:<16} {d:>4} ", .{ @tagName(op), constant });
    printValue(value);
    std.debug.print("\n", .{});

    // Print the closure values
    std.debug.assert(value == .object);
    std.debug.assert((value.object.* == .function));
    const function: *Function = &value.object.function;

    var i: u8 = 0;
    while (i < function.upvalueCount) : (i += 1) {
        const isLocal: bool = (chunk.code.items[offset].byte() == 1);
        const kind: []const u8 = if (isLocal) "local" else "upvalue";
        const idx: u8 = chunk.code.items[offset + 1].byte();
        std.debug.print("{d:0>4}      |    {s:<16} ", .{ offset, " " });
        std.debug.print("{s} {d}\n", .{ kind, idx });
        offset += 2;
    }

    return offset;
}

fn byteInstruction(op: OpCode, chunk: *const Chunk, offset: usize) usize {
    const slot: u8 = chunk.code.items[offset + 1].byte();
    std.debug.print("{s:<16} {d:>4}\n", .{ @tagName(op), slot });
    return offset + 2;
}

fn jumpInstruction(op: OpCode, sign: i8, chunk: *const Chunk, offset: usize) usize {
    var jump: u16 = (@as(u16, chunk.code.items[offset + 1].byte()) << 8) | chunk.code.items[offset + 2].byte();
    var dest: i64 = @intCast(offset + 3);
    dest += @as(i64, sign) * jump;
    std.debug.print("{s:<16} {d:>4} -> {d:<4}\n", .{ @tagName(op), offset, dest });
    return offset + 3;
}

fn simpleInstruction(op: OpCode, offset: usize) usize {
    std.debug.print("{s:<16}\n", .{@tagName(op)});
    return offset + 1;
}

pub fn printObject(obj: *Object) void {
    if (verbosity == .Verbose) {
        // More verbose type output for debugging
        switch (obj.*) {
            .string => |s| std.debug.print("Obj.String: '{s}'", .{s}),
            .upvalue => |_| std.debug.print("upvalue", .{}),
            .function => |f| {
                if (f.name) |str_obj| {
                    if (str_obj.string.len > 0) {
                        std.debug.print("Obj.Function: '{s}'", .{str_obj.string});
                    } else {
                        std.debug.print("Obj.Function: '<invalid>'", .{});
                    }
                } else {
                    std.debug.print("Obj.Function: '<script>", .{});
                }
            },
            .closure => |c| {
                printObject(c.obj); // the enclosed function object
            },
            .native => std.debug.print("<native fn>", .{}),
        }
    } else {
        // Normal output for normal usage
        switch (obj.*) {
            .string => |s| std.debug.print("'{s}'", .{s}),
            .upvalue => |_| std.debug.print("upvalue", .{}),
            .function => |f| {
                if (f.name) |str_obj| {
                    std.debug.print("<fn {s}>", .{str_obj.string});
                } else {
                    std.debug.print("<script>", .{});
                }
            },
            .closure => |f| {
                if (f.obj.function.name) |str_obj| {
                    std.debug.print("<fn {s}>", .{str_obj.string});
                } else {
                    std.debug.print("<script>", .{});
                }
            },
            .native => std.debug.print("<native fn>", .{}),
        }
    }
}

pub fn printValue(value: Value) void {
    switch (value) {
        .number => |num| std.debug.print("'{d:.3}'", .{num}),
        .bool => |b| std.debug.print("'{}'", .{b}),
        .none => std.debug.print("'none'", .{}),
        .object => |obj| printObject(obj),
    }
}
