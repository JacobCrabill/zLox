const std = @import("std");

const Allocator = std.mem.Allocator;
const File = std.fs.File;
const GPA = std.heap.GeneralPurposeAllocator(.{
    .stack_trace_frames = 10,
    .never_unmap = true,
    .retain_metadata = true,
});
const os = std.os;

const zlox = struct {
    usingnamespace @import("chunk.zig");
    usingnamespace @import("debug.zig");
    usingnamespace @import("vm.zig");
    usingnamespace @import("repl.zig");
};

const Chunk = zlox.Chunk;
const OpCode = zlox.OpCode;
const VM = zlox.VM;
const log = zlox.log;

pub fn main() !u8 {
    var gpa = GPA{};
    var alloc = gpa.allocator();
    defer _ = gpa.deinit(); // release all memory and check for leaks

    var args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len > 2) {
        log.err("Usage: {s} [file]", .{args[0]});
        return 65;
    } else if (args.len == 2) {
        try runFile(args[1], alloc);
    } else {
        var repl = zlox.makeRepl(alloc, std.io.getStdIn().reader(), std.io.getStdOut().writer());
        defer repl.deinit();
        try repl.start();
    }

    return 0;
}

fn runFile(path: []const u8, alloc: Allocator) !void {
    // Read file into memory
    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var realpath = try std.fs.realpath(path, &path_buf);
    var lox_file: File = try std.fs.openFileAbsolute(realpath, .{});
    var lox_text = try lox_file.readToEndAlloc(alloc, 1e9);

    var vm = VM.init(alloc);
    _ = vm.interpret(lox_text);
}

////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////

test "basic ops" {
    var alloc = std.testing.allocator;
    var vm = VM.init(alloc);
    var chunk = Chunk.init(alloc);
    defer chunk.deinit();

    const line: usize = 123;
    var constant: u8 = chunk.addConstant(.{ .value = 1.2 });
    try chunk.writeChunk(OpCode.OP_CONSTANT.byte(), line);
    try chunk.writeChunk(constant, line);

    constant = chunk.addConstant(.{ .value = 3.4 });
    try chunk.writeChunk(OpCode.OP_CONSTANT.byte(), line);
    try chunk.writeChunk(constant, line);

    try chunk.writeChunk(OpCode.OP_ADD.byte(), line);

    constant = chunk.addConstant(.{ .value = 5.6 });
    try chunk.writeChunk(OpCode.OP_CONSTANT.byte(), line);
    try chunk.writeChunk(constant, line);

    try chunk.writeChunk(OpCode.OP_DIVIDE.byte(), line);
    try chunk.writeChunk(OpCode.OP_NEGATE.byte(), line);

    try chunk.writeChunk(OpCode.OP_RETURN.byte(), line);
    zlox.disassembleChunk(&chunk, "test chunk");

    const res = vm.interpret(&chunk);
    log.info("result {any}", .{res});
    std.testing.expectEqual(zlox.InterpretResult.OK, res);
}
