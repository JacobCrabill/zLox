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
const BufSet = std.BufSet;
const ValueMap = std.StringHashMap(Value);

const Chunk = zlox.Chunk;
const Parser = zlox.Parser;
const Compiler = zlox.Compiler;
const OpCode = zlox.OpCode;
const Value = zlox.Value;
const Object = zlox.Object;
const NoneVal = zlox.NoneVal;
const TrueVal = zlox.TrueVal;
const FalseVal = zlox.FalseVal;

/// TODO: Convert to std.error enum; use Zig error handling
pub const LoxError = error{
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    pub const STACK_MAX: u32 = 256;
    alloc: Allocator,
    chunk: *Chunk,
    ip: usize,
    stack: [STACK_MAX]Value = undefined,
    stackTop: usize = 0,
    objects: ArrayList(*Object), // All heap allocs owned by the VM
    globals: ValueMap, // TODO: Custom hashmap with owned keys
    global_names: BufSet,
    strings: BufSet,

    /// Create a new Virtual Machine
    pub fn init(alloc: Allocator) VM {
        var vm = VM{
            .alloc = alloc,
            .chunk = undefined,
            .ip = 0,
            .objects = ArrayList(*Object).init(alloc),
            .globals = ValueMap.init(alloc),
            .global_names = BufSet.init(alloc),
            .strings = BufSet.init(alloc),
        };

        // We can't have uninitialized values, so init the stack
        for (&vm.stack) |*val| {
            val.* = NoneVal;
        }

        vm.resetStack();

        return vm;
    }

    /// Free all resources
    pub fn deinit(vm: *VM) void {
        // note: vm.chunk handled within interpret()
        for (vm.objects.items) |*obj| {
            obj.*.deinit(vm.alloc);
            vm.alloc.destroy(obj.*);
        }
        vm.objects.deinit();
        vm.globals.deinit();
        vm.global_names.deinit();
        vm.strings.deinit();
    }

    pub fn resetStack(vm: *VM) void {
        vm.stackTop = 0;
    }

    pub fn runtimeError(vm: *VM, comptime fmt: []const u8, args: anytype) void {
        const line: usize = vm.chunk.lines.items[vm.ip];
        std.debug.print("[line {d}] runtime error: ", .{line});
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});
        vm.resetStack();
    }

    pub fn interpret(vm: *VM, input: []const u8) !void {
        var compiler = try Parser.init(vm);
        defer compiler.deinit();
        var fn_obj = try compiler.compile(input);

        vm.chunk = &fn_obj.function.chunk;
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

    pub fn run(vm: *VM) !void {
        while (true) {
            if (builtin.mode == .Debug) {
                vm.printStack();
            }

            const op = vm.readByte();
            try vm.interpretOp(op);
            if (op == .OP_RETURN)
                return;
        }
    }

    fn interpretOp(vm: *VM, op: OpCode) !void {
        switch (op) {
            .OP_CONSTANT => {
                const val = vm.readConstant();
                vm.push(val); // TODO: Duplicate!!
            },
            .OP_NIL => vm.push(NoneVal),
            .OP_TRUE => vm.push(TrueVal),
            .OP_FALSE => vm.push(FalseVal),
            .OP_POP => _ = vm.pop(),
            .OP_GET_GLOBAL => {
                const name = try vm.readString();
                if (vm.globals.get(name)) |value| {
                    vm.push(value);
                } else {
                    vm.runtimeError("Undefined variable: '{s}'", .{name});
                    return LoxError.RUNTIME_ERROR;
                }
            },
            .OP_SET_GLOBAL => {
                const name = try vm.readString();
                if (vm.globals.contains(name)) {
                    const value = vm.peek(0);
                    try vm.globals.put(name, value);
                } else {
                    vm.runtimeError("Undefined variable: '{s}'", .{name});
                    return LoxError.RUNTIME_ERROR;
                }
            },
            .OP_GET_LOCAL => {
                const slot = vm.readByte().byte();
                std.debug.print("GET_LOCAL slot {d}: ", .{slot});
                zlox.printValue(vm.stack[slot]);
                vm.push(vm.stack[slot]);
            },
            .OP_SET_LOCAL => {
                const slot = vm.readByte().byte();
                vm.stack[slot] = vm.peek(0);
            },
            .OP_DEFINE_GLOBAL => try vm.defineGlobal(),
            .OP_EQUAL => {
                const a = vm.pop();
                const b = vm.pop();
                vm.push(Value{ .bool = zlox.valuesEqual(a, b) });
            },
            .OP_GREATER => try vm.binaryOp('>'),
            .OP_LESS => try vm.binaryOp('<'),
            .OP_ADD => try vm.binaryOp('+'),
            .OP_SUBTRACT => try vm.binaryOp('-'),
            .OP_MULTIPLY => try vm.binaryOp('*'),
            .OP_DIVIDE => try vm.binaryOp('/'),
            .OP_NOT => vm.push(Value{ .bool = zlox.isFalsey(vm.pop()) }),
            .OP_NEGATE => {
                if (vm.peek(0) != .number) {
                    vm.runtimeError("Operand must be a number", .{});
                    return LoxError.RUNTIME_ERROR;
                }
                const val = vm.pop();
                vm.push(Value{ .number = -val.number });
            },
            .OP_PRINT => {
                zlox.printValue(vm.pop());
                std.debug.print("\n", .{});
            },
            .OP_JUMP_IF_FALSE => {
                const offset: u16 = vm.readU16();
                if (zlox.isFalsey(vm.peek(0))) {
                    vm.ip += offset;
                }
            },
            .OP_JUMP => {
                const offset: u16 = vm.readU16();
                vm.ip += offset;
            },
            .OP_LOOP => {
                const offset: u16 = vm.readU16();
                vm.ip -= offset;
            },
            .OP_RETURN => return,
            else => {
                return LoxError.RUNTIME_ERROR;
            },
        }
    }

    fn readByte(vm: *VM) OpCode {
        const op = vm.chunk.code.items[vm.ip];
        vm.ip += 1;
        return op;
    }

    fn readU16(vm: *VM) u16 {
        var val: u16 = @as(u16, vm.chunk.code.items[vm.ip].byte()) << 8;
        val += vm.chunk.code.items[vm.ip + 1].byte();
        vm.ip += 2;
        return val;
    }

    /// Read a constant from the chunk, using the next instruction as the index
    fn readConstant(vm: *VM) Value {
        const idx = vm.readByte().byte();
        return vm.chunk.constants.items[idx];
    }

    /// Read a constant from the chunk as a string value
    fn readString(vm: *VM) ![]const u8 {
        const value = vm.readConstant();
        if (!zlox.isString(value)) {
            vm.runtimeError("Expected obj.string, got {s}", .{@tagName(value)});
            return error.InvalidType;
        }
        return value.object.string;
    }

    fn defineGlobal(vm: *VM) !void {
        // Use the global_names to store an owned copy of the identifier
        // The BufSet also safely de-dupes the given string
        const ident = try vm.readString();
        try vm.global_names.insert(ident);
        try vm.globals.put(vm.global_names.hash_map.getKey(ident).?, vm.pop());
    }

    fn binaryOp(vm: *VM, comptime op: u8) !void {
        if (zlox.isString(vm.peek(0)) and zlox.isString(vm.peek(1))) {
            if (op != '+') {
                vm.runtimeError("Invalid binary operator for strings: '{c}'", .{op});
                return LoxError.RUNTIME_ERROR;
            }

            vm.concatenate() catch {
                return LoxError.RUNTIME_ERROR;
            };
        } else if (vm.peek(0) == .number and vm.peek(1) == .number) {
            // Handle normal number binary operators
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
                    vm.run("ERROR: Invalid binary op: '{c}'\n", .{op});
                    vm.push(Value{ .number = std.math.nan(f64) });
                },
            }
        } else {
            vm.runtimeError("Binary operands must be two numbers or strings", .{});
            return LoxError.RUNTIME_ERROR;
        }
    }

    /// Concatenate two strings on top of the stack
    fn concatenate(vm: *VM) !void {
        var val_b = vm.pop();
        var val_a = vm.pop();

        const b: []const u8 = val_b.object.string;
        const a: []const u8 = val_a.object.string;
        const c = try std.mem.concat(vm.alloc, u8, &[_][]const u8{ a, b });

        // Transfer ownership of the concatenated string to a new object
        var obj = try zlox.takeString(vm, c);
        const str = Value{ .object = obj };

        vm.push(str);
    }

    fn printStack(vm: *VM) void {
        if (vm.stackTop > 0) {
            std.debug.print("          ", .{});
            for (0..vm.stackTop) |i| {
                const slot = vm.stack[i];
                std.debug.print("[ ", .{});
                zlox.printValue(slot);
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});
        }
    }
};
