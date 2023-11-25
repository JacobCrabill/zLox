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
const Closure = zlox.Closure;
const NativeFn = zlox.NativeFn;
const NoneVal = zlox.NoneVal;
const TrueVal = zlox.TrueVal;
const FalseVal = zlox.FalseVal;

/// TODO: Convert to std.error enum; use Zig error handling
pub const LoxError = zlox.Error;

pub const CallFrame = struct {
    obj: *Object = undefined, // Pointer to the VM-owned Closure object
    ip: usize = 0, // Instruction 'pointer' into the function's code chunk
    slots: []Value = undefined, // Local stack for the call frame.  Points into VM's global stack.
    rbsp: usize = 0, // Stack base pointer prior to adding this CallFrame
    chunk: *Chunk = undefined, // Pointer to the function's Chunk

    /// Initialize a CallFrame
    /// 'function' must be a heap-allocated Function object
    pub fn init(frame: *CallFrame, closure: *Object, stack: []Value, stack_base: usize) void {
        std.debug.assert(closure.* == zlox.ObjType.closure);
        frame.obj = closure;
        frame.ip = 0;
        frame.slots = stack;
        frame.rbsp = stack_base;
        frame.chunk = &closure.closure.obj.function.chunk;
    }

    pub fn deinit(frame: *CallFrame) void {
        _ = frame;
    }
};

pub const VM = struct {
    pub const FRAMES_MAX: u32 = 64;
    pub const STACK_MAX: u32 = FRAMES_MAX * 256;
    alloc: Allocator,
    stack: [STACK_MAX]Value = undefined,
    stackTop: usize = 0,
    frames: [FRAMES_MAX]CallFrame = undefined,
    frameCount: u8 = 0,
    objects: ArrayList(*Object), // All heap-allocated Objects owned by the VM
    globals: ValueMap, // TODO: Custom hashmap with owned keys
    global_names: BufSet,
    strings: BufSet,

    /// Create a new Virtual Machine
    pub fn init(alloc: Allocator) VM {
        var vm = VM{
            .alloc = alloc,
            .frameCount = 0,
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

        vm.defineNative("clock", clockNative) catch |err| {
            std.debug.print("Error defining native function: {any}\n", .{err});
        };

        vm.defineNative("str", strBuiltin) catch |err| {
            std.debug.print("Error defining native function: {any}\n", .{err});
        };

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
        std.debug.print(fmt ++ "\n", args);

        var i = vm.frameCount;
        while (i > 0) : (i -= 1) {
            const frame = &vm.frames[i - 1];
            const line: usize = frame.chunk.lines.items[frame.ip - 1];
            const name = frame.obj.closure.obj.function.getName();
            std.debug.print("[line {d}] in {s}\n", .{ line, name });
        }
        vm.resetStack();
    }

    pub fn interpret(vm: *VM, input: []const u8) !void {
        var parser = try Parser.init(vm);
        defer parser.deinit();

        var fn_obj = try parser.compile(input);

        vm.push(Value{ .object = fn_obj });
        var closure = try zlox.newClosure(vm, fn_obj);
        _ = vm.pop();
        vm.push(Value{ .object = closure });
        try vm.call(closure, 0);

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
            if (zlox.verbosity == .Verbose) {
                vm.printStack();
            }

            const op = vm.readByte();
            vm.interpretOp(op) catch |err| {
                if (err == LoxError.OK) {
                    // Program exited normally
                    return;
                }
                return err;
            };
        }
    }

    fn interpretOp(vm: *VM, op: OpCode) !void {
        var frame = vm.currentFrame();
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
                vm.push(frame.slots[slot]);
            },
            .OP_SET_LOCAL => {
                const slot = vm.readByte().byte();
                frame.slots[slot] = vm.peek(0);
            },
            .OP_DEFINE_GLOBAL => try vm.defineGlobal(),
            .OP_GET_UPVALUE => {
                const slot: u8 = vm.readByte().byte();
                vm.push(vm.currentFrame().obj.closure.upvalues.items[slot].upvalue.location.*);
            },
            .OP_SET_UPVALUE => {
                const slot: u8 = vm.readByte().byte();
                vm.currentFrame().obj.closure.upvalues.items[slot].upvalue.location.* = vm.peek(0);
            },
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
                    vm.currentFrame().ip += offset;
                }
            },
            .OP_JUMP => {
                const offset: u16 = vm.readU16();
                vm.currentFrame().ip += offset;
            },
            .OP_LOOP => {
                const offset: u16 = vm.readU16();
                vm.currentFrame().ip -= offset;
            },
            .OP_CALL => {
                const argc = vm.readByte().byte();
                try vm.callValue(vm.peek(argc), argc);
            },
            .OP_CLOSURE => {
                var fn_obj: *Object = vm.readConstant().object;
                var obj_closure = try zlox.newClosure(vm, fn_obj);
                vm.push(Value{ .object = obj_closure });

                var closure: *Closure = &obj_closure.closure;
                var i: u8 = 0;
                while (i < closure.upvalueCount) : (i += 1) {
                    const isLocal: bool = (vm.readByte().byte() == 1);
                    const idx: u8 = vm.readByte().byte();
                    var upvalue: *Object = undefined;
                    if (isLocal) {
                        upvalue = try zlox.captureUpvalue(vm, &vm.currentFrame().slots[idx]);
                    } else {
                        upvalue = vm.currentFrame().obj.closure.upvalues.items[idx];
                    }
                    closure.upvalues.appendAssumeCapacity(upvalue);
                }
            },
            .OP_RETURN => {
                var result = vm.pop();
                vm.frameCount -= 1;
                if (vm.frameCount == 0) {
                    _ = vm.pop();
                    return LoxError.OK;
                }

                // Unroll the function stack
                vm.stackTop = frame.rbsp;
                vm.push(result);
            },
            else => {
                return LoxError.RUNTIME_ERROR;
            },
        }
    }

    fn currentFrame(vm: *VM) *CallFrame {
        std.debug.assert(vm.frameCount < VM.FRAMES_MAX);
        return &vm.frames[vm.frameCount - 1];
    }

    fn readByte(vm: *VM) OpCode {
        var frame: *CallFrame = vm.currentFrame();
        const op = frame.chunk.code.items[frame.ip];
        frame.ip += 1;
        return op;
    }

    fn readU16(vm: *VM) u16 {
        var frame: *CallFrame = vm.currentFrame();
        var val: u16 = @as(u16, frame.chunk.code.items[frame.ip].byte()) << 8;
        val += frame.chunk.code.items[frame.ip + 1].byte();
        frame.ip += 2;
        return val;
    }

    /// Read a constant from the chunk, using the next instruction as the index
    fn readConstant(vm: *VM) Value {
        const idx = vm.readByte().byte();
        return vm.currentFrame().chunk.constants.items[idx];
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

    /// Assume the given Value is a function to be called.
    /// Return false if it is not a function, or if calling it fails.
    fn callValue(vm: *VM, callee: Value, argc: u8) !void {
        switch (callee) {
            .object => |obj| {
                switch (obj.*) {
                    .closure => |_| {
                        try vm.call(obj, argc);
                        return;
                    },
                    .native => |native| {
                        const result: Value = try native(vm, argc, vm.stack[vm.stackTop - argc ..]);
                        vm.stackTop -= argc + 1;
                        vm.push(result);
                        return;
                    },
                    else => {},
                }
            },
            else => {},
        }

        vm.runtimeError("Can only call function closures and classes", .{});
        return LoxError.RUNTIME_ERROR;
    }

    /// Call the given Closure's Function object (the Object is already known to be a Closure)
    fn call(vm: *VM, obj: *Object, argc: u8) !void {
        var function: *zlox.Function = &obj.closure.obj.function;
        if (argc != function.arity) {
            vm.runtimeError("Function {s} expected {d} arguments but given {d}", .{
                function.getName(),
                function.arity,
                argc,
            });
            return LoxError.RUNTIME_ERROR;
        }

        if (vm.frameCount == VM.FRAMES_MAX) {
            vm.runtimeError("Stack overflow", .{});
            return LoxError.RUNTIME_ERROR;
        }

        var frame: *CallFrame = &vm.frames[vm.frameCount];
        vm.frameCount += 1;
        const rbsp = vm.stackTop - argc - 1;
        frame.init(obj, vm.stack[rbsp..], rbsp);
    }

    /// Add a native function to our globals
    fn defineNative(vm: *VM, name: []const u8, native: NativeFn) !void {
        vm.push(Value{ .object = try zlox.copyString(vm, name) });
        vm.push(Value{ .object = try zlox.newNative(vm, native) });
        try vm.globals.put(vm.peek(1).object.string, vm.peek(0));
        _ = vm.pop();
        _ = vm.pop();
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

///////////////////////////////////////////////////////////////////////////////
/// Native Functions
///////////////////////////////////////////////////////////////////////////////

/// Get the current system time in seconds
pub fn clockNative(vm: *VM, argc: u8, argv: []Value) zlox.Error!Value {
    _ = vm;
    _ = argc;
    _ = argv;
    var micros: f64 = @floatFromInt(std.time.microTimestamp());
    return Value{ .number = micros / 1e6 };
}

pub fn strBuiltin(vm: *VM, argc: u8, argv: []Value) zlox.Error!Value {
    if (argc != 1) return NoneVal;
    const val = argv[0];
    var str: []const u8 = switch (val) {
        .number => |num| try std.fmt.allocPrint(vm.alloc, "'{d:.3}'", .{num}),
        .bool => |b| try std.fmt.allocPrint(vm.alloc, "'{}'", .{b}),
        .none => try std.fmt.allocPrint(vm.alloc, "'none'", .{}),
        .object => |obj| blk: {
            if (obj.* == zlox.ObjType.string) {
                // If it's already a String object, just return the Value
                return val;
            } else {
                break :blk try strObject(vm.alloc, obj.*);
            }
        },
    };
    return Value{ .object = try zlox.createString(vm, str) };
}

/// Convert an Object to a string Object
pub fn strObject(alloc: Allocator, obj: Object) zlox.Error![]const u8 {
    switch (obj) {
        .function => |f| {
            if (f.name) |str_obj| {
                return try std.fmt.allocPrint(alloc, "<fun {s}>", .{str_obj.string});
            } else {
                return try std.fmt.allocPrint(alloc, "<script>", .{});
            }
        },
        .closure => |f| {
            if (f.obj.function.name) |str_obj| {
                return try std.fmt.allocPrint(alloc, "<fun {s}>", .{str_obj.string});
            } else {
                return try std.fmt.allocPrint(alloc, "<script>", .{});
            }
        },
        .native => return try std.fmt.allocPrint(alloc, "<native fn>", .{}),
        else => return LoxError.RUNTIME_ERROR, // can't happen
    }
}
