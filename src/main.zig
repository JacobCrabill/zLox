const std = @import("std");
const chunks = @import("chunk.zig");

const Chunk = chunks.Chunk;
const OpCode = chunks.OpCode;
const Debug = @import("debug.zig");

const GPA = std.heap.GeneralPurposeAllocator(.{});

const log = Debug.log;

pub fn main() !u8 {
    var gpa = GPA{};
    var alloc = gpa.allocator();

    var chunk = Chunk.init(alloc);
    defer chunk.deinit();

    const constant: u8 = chunk.addConstant(.{ .value = 1.2 });
    try chunk.writeChunk(OpCode.OP_CONSTANT.byte(), 123);
    try chunk.writeChunk(constant, 123);

    try chunk.writeChunk(OpCode.OP_RETURN.byte(), 123);
    Debug.disassembleChunk(&chunk, "test chunk");

    // var args = std.process.argsAlloc(alloc);

    // if (args.len > 2) {
    //     log.err("Usage: {s} [file]", .{args[0]});
    //     return 65;
    // } else if (args.len == 1) {
    //     // runFile(args[1]);
    // } else {
    //     // repl()
    // }

    return 0;
}
