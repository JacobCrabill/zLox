const std = @import("std");

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
    object: Object,
};

pub const Object = union(ObjectType) {
    string: []const u8,
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
