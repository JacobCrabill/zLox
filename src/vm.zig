const std = @import("std");
const builtin = @import("builtin");

const zlox = struct {
    usingnamespace @import("chunk.zig");
    usingnamespace @import("debug.zig");
    usingnamespace @import("value.zig");
    usingnamespace @import("compiler.zig");
};

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Chunk = zlox.Chunk;
const Compiler = zlox.Compiler;
const OpCode = zlox.OpCode;
const Value = zlox.Value;

pub const InterpretResult = enum(u8) {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    pub const STACK_MAX: u32 = 256;
    alloc: Allocator,
    chunk: *const Chunk,
    ip: usize,
    stack: [STACK_MAX]Value = undefined,
    stackTop: usize = 0,

    pub fn init(alloc: Allocator) VM {
        var vm = VM{
            .alloc = alloc,
            .chunk = undefined,
            .ip = 0,
        };

        // We can't have uninitialized values, so init the stack
        for (&vm.stack) |*val| {
            val.* = Value{};
        }

        vm.resetStack();

        return vm;
    }

    pub fn deinit(vm: *VM) void {
        _ = vm;
    }

    pub fn resetStack(vm: *VM) void {
        vm.stackTop = 0;
    }

    pub fn interpret(vm: *VM, input: []const u8) InterpretResult {
        var chunk = Chunk.init(vm.alloc);
        defer chunk.deinit();

        var compiler = Compiler.init(vm.alloc);
        compiler.compile(input, &chunk) catch return .COMPILE_ERROR;

        vm.chunk = &chunk;
        vm.ip = 0;

        return vm.run();
    }

    pub fn push(vm: *VM, value: Value) void {
        vm.stack[vm.stackTop] = value;
        vm.stackTop += 1;
    }

    pub fn pop(vm: *VM) Value {
        vm.stackTop -= 1;
        return vm.stack[vm.stackTop];
    }

    pub fn run(vm: *VM) InterpretResult {
        while (true) {
            if (builtin.mode == .Debug) {
                std.debug.print("          ", .{});
                for (0..vm.stackTop) |i| {
                    const slot = vm.stack[i];
                    std.debug.print("[ ", .{});
                    zlox.printValue(slot);
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
            }

            _ = zlox.disassembleInstruction(vm.chunk, vm.ip);
            const op = vm.readByte();
            switch (op) {
                .OP_CONSTANT => {
                    const val = vm.readConstant();
                    vm.push(val);
                },
                .OP_ADD => vm.binaryOp('+'),
                .OP_SUBTRACT => vm.binaryOp('-'),
                .OP_MULTIPLY => vm.binaryOp('*'),
                .OP_DIVIDE => vm.binaryOp('/'),
                .OP_NEGATE => {
                    const val = vm.pop();
                    vm.push(Value{ .value = -val.value });
                },
                .OP_RETURN => {
                    zlox.printValue(vm.pop());
                    std.debug.print("\n", .{});
                    return .OK;
                },
                else => {
                    return .RUNTIME_ERROR;
                },
            }
        }
    }

    fn readByte(vm: *VM) OpCode {
        const op = vm.chunk.code.items[vm.ip];
        vm.ip += 1;
        return op;
    }

    fn readConstant(vm: *VM) Value {
        const idx = vm.readByte().byte();
        return vm.chunk.constants.items[idx];
    }

    fn binaryOp(vm: *VM, comptime op: u8) void {
        const b: Value = vm.pop();
        const a: Value = vm.pop();
        vm.push(Value{
            .value = switch (op) {
                '+' => a.value + b.value,
                '-' => a.value - b.value,
                '*' => a.value * b.value,
                '/' => a.value / b.value,
                else => blk: {
                    std.debug.print("ERROR: Invalid binary op: '{c}'\n", .{op});
                    break :blk std.math.nan(f64);
                },
            },
        });
    }
};
