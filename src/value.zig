const std = @import("std");

const Allocator = std.mem.Allocator;

pub const ValueType = enum(u8) {
    bool,
    none,
    number,
    object,
};

pub const ObjectType = enum(u8) {
    string,
};

pub const Value = union(ValueType) {
    bool: bool,
    none: void,
    number: f64,
    object: Object, // *ALL* objects live on the heap!

    pub fn deinit(value: *Value, alloc: Allocator) void {
        switch (value.*) {
            .object => |*obj| obj.deinit(alloc),
            else => {},
        }
    }
};

pub const Object = union(ObjectType) {
    string: []const u8,

    pub fn deinit(obj: *Object, alloc: Allocator) void {
        _ = alloc;
        switch (obj.*) {
            // For now, we are letting the VM's 'strings' BufSet manage all strings
            //   (this just makes life easier)
            // In the future, a custom strings hash map may transfer ownership
            // back to the Object
            .string => {}, // alloc.free(obj.string),
        }
    }
};

pub const NoneVal: Value = Value{ .none = {} };
pub const TrueVal: Value = Value{ .bool = true };
pub const FalseVal: Value = Value{ .bool = false };

pub fn boolVal(value: bool) Value {
    return .{ .bool = value };
}

pub fn numberVal(value: f64) Value {
    return .{ .number = value };
}

pub fn isFalsey(value: Value) bool {
    return switch (value) {
        .none => true,
        .bool => |b| !b,
        else => false,
    };
}

pub fn isString(value: Value) bool {
    if (value != .object) return false;
    return (value.object == .string);
}

pub fn valuesEqual(a: Value, b: Value) bool {
    if (@intFromEnum(a) != @intFromEnum(b))
        return false;

    switch (a) {
        .bool => return a.bool == b.bool,
        .number => return a.number == b.number,
        .none => return true,
        .object => return objectsEqual(a.object, b.object),
    }
}

pub fn objectsEqual(a: Object, b: Object) bool {
    if (@intFromEnum(a) != @intFromEnum(b))
        return false;

    switch (a) {
        .string => |s| return std.mem.eql(u8, s, b.string),
    }
}
