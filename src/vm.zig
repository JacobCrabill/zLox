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
const NoneVal = zlox.NoneVal;
const TrueVal = zlox.TrueVal;
const FalseVal = zlox.FalseVal;

/// TODO: Convert to std.error enum; use Zig error handling
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
            val.* = NoneVal;
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

    pub fn runtimeError(vm: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        const line: usize = vm.chunk.lines.items[vm.ip];
        std.debug.print("[line {d}] in script\n", .{line});
        vm.resetStack();
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

    pub fn peek(vm: *VM, distance: usize) Value {
        return vm.stack[vm.stackTop - 1 - distance];
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

            const op = vm.readByte();
            // TODO: Replace with Zig error enum; use try / catch
            const res: InterpretResult = vm.interpretOp(op);
            if (res != .OK or op == .OP_RETURN)
                return res;
        }
    }

    fn interpretOp(vm: *VM, op: OpCode) InterpretResult {
        switch (op) {
            .OP_CONSTANT => {
                const val = vm.readConstant();
                vm.push(val);
            },
            .OP_NIL => vm.push(NoneVal),
            .OP_TRUE => vm.push(TrueVal),
            .OP_FALSE => vm.push(FalseVal),
            .OP_EQUAL => {
                const a = vm.pop();
                const b = vm.pop();
                vm.push(Value{ .bool = zlox.valuesEqual(a, b) });
            },
            .OP_GREATER => return vm.binaryOp('>'),
            .OP_LESS => return vm.binaryOp('<'),
            .OP_ADD => return vm.binaryOp('+'),
            .OP_SUBTRACT => return vm.binaryOp('-'),
            .OP_MULTIPLY => return vm.binaryOp('*'),
            .OP_DIVIDE => return vm.binaryOp('/'),
            .OP_NOT => vm.push(Value{ .bool = zlox.isFalsey(vm.pop()) }),
            .OP_NEGATE => {
                if (vm.peek(0) != .number) {
                    vm.runtimeError("Operand must be a number", .{});
                    return .RUNTIME_ERROR;
                }
                const val = vm.pop();
                vm.push(Value{ .number = -val.number });
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

        return .OK;
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

    fn binaryOp(vm: *VM, comptime op: u8) InterpretResult {
        if (vm.peek(0) != .number or vm.peek(1) != .number) {
            vm.runtimeError("Binary operands must be numbers", .{});
            return .RUNTIME_ERROR;
        }

        const b: f64 = vm.pop().number;
        const a: f64 = vm.pop().number;

        switch (op) {
            '+' => vm.push(Value{ .number = a + b }),
            '-' => vm.push(Value{ .number = a - b }),
            '*' => vm.push(Value{ .number = a * b }),
            '/' => vm.push(Value{ .number = a / b }),
            '>' => vm.push(Value{ .bool = a > b }),
            '<' => vm.push(Value{ .bool = a < b }),
            else => {
                std.debug.print("ERROR: Invalid binary op: '{c}'\n", .{op});
                vm.push(Value{ .number = std.math.nan(f64) });
            },
        }

        return .OK;
    }
};
