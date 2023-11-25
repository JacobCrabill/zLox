const std = @import("std");
const VM = @import("vm.zig").VM;
const Chunk = @import("chunk.zig").Chunk;

const Allocator = std.mem.Allocator;

/// Enum for Value types
pub const ValueType = enum(u8) {
    bool,
    none,
    number,
    object,
};

/// Enum for Object types
pub const ObjType = enum(u8) {
    string,
    function,
    closure,
    native,
};

/// The base type of all values in Lox
pub const Value = union(ValueType) {
    bool: bool,
    none: void,
    number: f64,
    object: *Object, // *ALL* objects live on the heap!

    pub fn deinit(value: *Value, alloc: Allocator) void {
        switch (value.*) {
            .object => |obj| obj.deinit(alloc),
            else => {},
        }
    }
};

/// Objects are Values which live on the heap
pub const Object = union(ObjType) {
    string: []const u8,
    function: Function,
    closure: Closure,
    native: NativeFn,

    pub fn deinit(obj: *Object, alloc: Allocator) void {
        switch (obj.*) {
            .string => alloc.free(obj.string),
            .function => |*f| f.chunk.deinit(),
            else => {},
        }
    }
};

pub const Function = struct {
    arity: usize = 0,
    chunk: Chunk = undefined,
    name: ?*Object = null, // String object containing the function name (if not script)

    pub fn getName(fun: Function) []const u8 {
        if (fun.name) |obj| {
            return obj.string;
        }

        return "script";
    }
};

pub const Closure = struct {
    obj: *Object = undefined, // Function object pointer
};

pub const Error = error{
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
    OutOfMemory, // zig std lib
};

pub const NativeFn = *const fn (vm: *VM, argc: u8, argv: []Value) Error!Value;

pub const NoneVal: Value = Value{ .none = {} };
pub const TrueVal: Value = Value{ .bool = true };
pub const FalseVal: Value = Value{ .bool = false };

/// Make a boolean Value
pub fn boolVal(value: bool) Value {
    return .{ .bool = value };
}

/// Make a number Value
pub fn numberVal(value: f64) Value {
    return .{ .number = value };
}

/// Check if a Value can evaluate to 'false'
pub fn isFalsey(value: Value) bool {
    return switch (value) {
        .none => true,
        .bool => |b| !b,
        else => false,
    };
}

/// Check if the Value is an Object.string
pub fn isString(value: Value) bool {
    if (value != .object) return false;
    return (value.object.* == .string);
}

/// Compare two Values
pub fn valuesEqual(a: Value, b: Value) bool {
    if (@intFromEnum(a) != @intFromEnum(b))
        return false;

    switch (a) {
        .bool => return a.bool == b.bool,
        .number => return a.number == b.number,
        .none => return true,
        .object => return objectsEqual(a.object.*, b.object.*),
    }
}

/// Compare two Objects
pub fn objectsEqual(a: Object, b: Object) bool {
    if (@intFromEnum(a) != @intFromEnum(b))
        return false;

    switch (a) {
        .string => |s| return std.mem.eql(u8, s, b.string),
        .function => |*f| return functionsEqual(f, &b.function),
        .closure => |c| return functionsEqual(&c.obj.function, &b.closure.obj.function),
        .native => |native| {
            return @intFromPtr(native) == @intFromPtr(b.native);
        },
    }
}

/// Compare two Function objects (by name)
fn functionsEqual(a: *const Function, b: *const Function) bool {
    // Empty names mean script scope
    if (a.name) |aname| {
        if (b.name) |bname| {
            return std.mem.eql(u8, aname.string, bname.string);
        } else {
            return false;
        }
    }
    if (b.name) |_| return false;
    return true;
}

/// Allocate a new Object on the heap, adding it to our global objects list
/// The object is created on the stack, but copied to the heap
pub fn createObject(vm: *VM, obj: Object) !*Object {
    var new_obj = try vm.alloc.create(Object);
    new_obj.* = obj;
    try vm.objects.append(new_obj);
    return vm.objects.getLast();
}

/// Allocate a new Object on the heap of type 'Function'
/// Initializes the function's chunk using the VM's allocator
pub fn newFunction(vm: *VM) !*Object {
    var fun = Function{ .chunk = Chunk.init(vm.alloc) };
    return try createObject(vm, Object{ .function = fun });
}

/// Allocate a new Object on the heap of type 'Closure'
/// The new closure takes ownership of the given Function object
pub fn newClosure(vm: *VM, function: *Object) !*Object {
    return try createObject(vm, Object{ .closure = .{ .obj = function } });
}

/// Allocate a new Object on the heap of type 'NativeFn'
pub fn newNative(vm: *VM, native: NativeFn) !*Object {
    return try createObject(vm, Object{ .native = native });
}

/// Allocates an Object of type 'string' on the heap, transferring ownership
/// of the given string to the new Object
pub fn createString(vm: *VM, str: []const u8) !*Object {
    return try createObject(vm, Object{ .string = str });
}

/// Copy the given string into a new Object on the heap
pub fn copyString(vm: *VM, str: []const u8) !*Object {
    var copy = try vm.alloc.dupe(u8, str);
    return try createString(vm, copy);
}

/// Pass ownership of the given string to a new Object on the heap
pub fn takeString(vm: *VM, str: []const u8) !*Object {
    return try createString(vm, str);
}
