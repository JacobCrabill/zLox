const std = @import("std");
const VM = @import("vm.zig").VM;

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

    pub fn deinit(obj: *Object, alloc: Allocator) void {
        switch (obj.*) {
            .string => alloc.free(obj.string),
        }
    }
};

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
    }
}

/// Allocate a new Object on the heap, adding it to our global objects list
pub fn createObject(vm: *VM, obj: Object) !*Object {
    var new_obj = try vm.alloc.create(Object);
    new_obj.* = obj;
    try vm.objects.append(new_obj);
    return vm.objects.getLast();
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
