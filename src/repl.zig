const std = @import("std");
const builtin = @import("builtin");

const zlox = struct {
    usingnamespace @import("chunk.zig");
    usingnamespace @import("debug.zig");
    usingnamespace @import("vm.zig");
};

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const VM = zlox.VM;

pub fn makeRepl(alloc: Allocator, input: anytype, output: anytype) Repl(@TypeOf(input), @TypeOf(output)) {
    return Repl(@TypeOf(input), @TypeOf(output)).init(alloc, input, output);
}

pub fn Repl(comptime InStream: type, comptime OutStream: type) type {
    return struct {
        const Self = @This();
        alloc: Allocator,
        input: InStream,
        output: OutStream,
        max_input_size: usize = 65535,
        commands: ArrayList([]const u8),
        should_exit: bool = false,

        // Create
        pub fn init(alloc: Allocator, input: InStream, output: OutStream) Self {
            return Self{
                .alloc = alloc,
                .input = input,
                .output = output,
                .commands = ArrayList([]const u8).init(alloc),
            };
        }

        pub fn deinit(self: *Self) void {
            for (self.commands.items) |*str| {
                self.alloc.free(str.*);
            }
            self.commands.deinit();
        }

        // Run the REPL
        pub fn start(self: *Self) !void {
            try self.output.print("Welcome to zLox!\n", .{});
            try self.output.print("  version: 0.0.1\n", .{});

            var vm = VM.init(self.alloc);
            defer vm.deinit();

            while (!self.should_exit) {
                try self.output.print(">> ", .{});
                const raw_input = (try self.nextLine()).?;
                var input: []u8 = try self.alloc.alloc(u8, raw_input.len);
                @memcpy(input, raw_input);
                try self.commands.append(input);

                // Check for special commands handled by the REPL
                if (self.handleInput(input))
                    continue;

                // TODO: run!
                const res = vm.interpret(input);
                if (res != .OK) {
                    std.debug.print("ERROR: {any}\n", .{res});
                }

                try self.output.print("\n", .{});
            }
        }

        pub fn runOnce(self: *Self, input: []const u8) !void {
            _ = input;
            // var lex = Lexer.init(input);
            try self.output.print("\n", .{});
        }

        fn nextLine(self: *Self) !?[]const u8 {
            var line = (try self.input.readUntilDelimiterOrEofAlloc(
                self.alloc,
                '\n',
                self.max_input_size,
            )) orelse return null;

            // trim annoying windows-only carriage return character
            if (builtin.os.tag == .windows) {
                return std.mem.trimRight(u8, line, "\r");
            } else {
                return line;
            }
        }

        fn handleInput(self: *Self, input: []const u8) bool {
            if (std.mem.startsWith(u8, input, "exit") or std.mem.startsWith(u8, input, "quit")) {
                self.should_exit = true;
                return true;
            }
            return false;
        }
    };
}
