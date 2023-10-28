const std = @import("std");
const builtin = @import("builtin");

const zlox = struct {
    usingnamespace @import("lexer.zig");
    usingnamespace @import("chunk.zig");
    usingnamespace @import("debug.zig");
    usingnamespace @import("tokens.zig");
    usingnamespace @import("value.zig");
};

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lexer = zlox.Lexer;
const Chunk = zlox.Chunk;
const OpCode = zlox.OpCode;
const Token = zlox.Token;
const TokenType = zlox.TokenType;
const Value = zlox.Value;

// zig fmt: off
pub const Precedence = enum(u8) {
    NONE,
    ASSIGNMENT, // =
    OR,         // or
    AND,        // and
    EQUALITY,   // == !=
    COMPARISON, // < > <= >=
    TERM,       // + -
    FACTOR,     // * /
    UNARY,      // ! -
    CALL,       // . ()
    PRIMARY,
};
// zig fmt: on

pub const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .NONE,

    pub fn init(pre: ?ParseFn, in: ?ParseFn, prec: Precedence) ParseRule {
        return .{ .prefix = pre, .infix = in, .precedence = prec };
    }
};

// Type alias for a Pratt parser function
const ParseFn = *const fn (*Compiler) void;

pub const Compiler = struct {
    const Self = @This();
    alloc: Allocator,
    current: Token,
    previous: Token,
    lexer: Lexer = undefined,
    hadError: bool = false,
    panicMode: bool = false,
    chunk: *Chunk = undefined,

    pub fn init(alloc: Allocator) Self {
        return .{
            .alloc = alloc,
            .current = undefined,
            .previous = undefined,
        };
    }

    pub fn compile(self: *Self, input: []const u8, chunk: *Chunk) !void {
        self.lexer = Lexer.init(input);
        // defer self.lexer.deinit();

        self.chunk = chunk;
        self.hadError = false;
        self.panicMode = false;

        self.advance();
        self.expression();

        while (self.lexer.scanToken()) |token| {
            std.debug.print("{any}\n", .{token});
            if (token.kind == .EOF) break;
        }

        self.endCompiler();

        if (self.hadError) return error.CompileError;
    }

    fn advance(self: *Self) void {
        self.previous = self.current;

        while (true) {
            self.current = self.lexer.scanToken() orelse {
                self.errorAtCurrent("Unable to parse token");
                break;
            };

            if (self.current.kind != .ERROR) break;

            // If we encountered an error, keep going and try to recover
            self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Self, kind: TokenType, error_msg: []const u8) void {
        if (self.current.kind == kind) {
            self.advance();
            return;
        }

        self.errorAtCurrent(error_msg);
    }

    /// Emit a single byte to the current Chunk being compiled
    fn emitByte(self: *Self, byte: u8) void {
        self.chunk.writeChunk(byte, self.previous.line) catch {
            self.errorAt(&self.previous, "ERROR: Out of stack space?");
        };
    }

    fn emitBytes(self: *Self, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn emitReturn(self: *Self) void {
        self.emitByte(OpCode.OP_RETURN.byte());
    }

    fn makeConstant(self: *Self, value: Value) u8 {
        const idx = self.chunk.addConstant(value);
        if (idx > std.math.maxInt(u8)) {
            self.errorMsg("Too many constants in one chunk!");
            return 0;
        }

        return idx;
    }

    fn emitConstant(self: *Self, value: Value) void {
        self.emitBytes(OpCode.OP_CONSTANT.byte(), self.makeConstant(value));
    }

    fn endCompiler(self: *Self) void {
        self.emitReturn();
        if (builtin.mode == .Debug and !self.hadError) {
            zlox.disassembleChunk(self.chunk, "code");
        }
    }

    fn binary(self: *Self) void {
        const optype: TokenType = self.previous.kind;
        var rule: ParseRule = getParseRule(optype);
        self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (optype) {
            .PLUS => self.emitByte(OpCode.OP_ADD.byte()),
            .MINUS => self.emitByte(OpCode.OP_SUBTRACT.byte()),
            .STAR => self.emitByte(OpCode.OP_MULTIPLY.byte()),
            .SLASH => self.emitByte(OpCode.OP_DIVIDE.byte()),
            .BANG_EQUAL => self.emitBytes(OpCode.OP_EQUAL.byte(), OpCode.OP_NOT.byte()),
            .EQUAL_EQUAL => self.emitByte(OpCode.OP_EQUAL.byte()),
            .GREATER => self.emitByte(OpCode.OP_GREATER.byte()),
            .GREATER_EQUAL => self.emitBytes(OpCode.OP_LESS.byte(), OpCode.OP_NOT.byte()),
            .LESS => self.emitByte(OpCode.OP_LESS.byte()),
            .LESS_EQUAL => self.emitBytes(OpCode.OP_GREATER.byte(), OpCode.OP_NOT.byte()),
            else => return,
        }
    }

    fn literal(self: *Self) void {
        switch (self.previous.kind) {
            .TRUE => self.emitByte(OpCode.OP_TRUE.byte()),
            .FALSE => self.emitByte(OpCode.OP_FALSE.byte()),
            .NIL => self.emitByte(OpCode.OP_NIL.byte()),
            else => {},
        }
    }

    pub fn grouping(self: *Self) void {
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after expression");
    }

    pub fn number(self: *Self) void {
        const value: f64 = std.fmt.parseFloat(f64, self.previous.lexeme) catch 0;
        self.emitConstant(Value{ .number = value });
    }

    fn unary(self: *Self) void {
        const optype: TokenType = self.previous.kind;

        self.parsePrecedence(.UNARY);

        switch (optype) {
            .MINUS => self.emitByte(OpCode.OP_NEGATE.byte()),
            .BANG => self.emitByte(OpCode.OP_NOT.byte()),
            else => return,
        }
    }

    fn prefixExpr(self: *Self, kind: TokenType) void {
        switch (kind) {
            .MINUS => self.unary(.TERM),
            .NUMBER => self.number(),
            else => {},
        }
    }

    fn parsePrecedence(self: *Self, prec: Precedence) void {
        self.advance();
        if (getParseRule(self.previous.kind).prefix) |*prefixRule| {
            prefixRule.*(self);
        } else {
            self.errorMsg("Invalid expression");
            return;
        }

        while (@intFromEnum(prec) <= @intFromEnum(getParseRule(self.current.kind).precedence)) {
            self.advance();
            if (getParseRule(self.previous.kind).infix) |infixRule| {
                infixRule(self);
            }
        }
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.ASSIGNMENT);
    }

    // Error Handling Utility Functions

    fn errorAtCurrent(self: *Self, msg: []const u8) void {
        self.errorAt(&self.current, msg);
    }

    fn errorAt(self: *Self, token: *const Token, msg: []const u8) void {
        if (self.panicMode) return;
        self.panicMode = true;
        std.debug.print("[line {d}] Error", .{token.line});

        if (token.kind == .EOF) {
            std.debug.print(" at end", .{});
        } else if (token.kind == .ERROR) {
            // Nothing
        } else {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{msg});
        self.hadError = true;
    }

    fn errorMsg(self: *Self, msg: []const u8) void {
        self.errorAt(&self.previous, msg);
    }
};

/// Get the Pratt parser rule for the given TokenType
///
/// The default rule for unhandled token types contains null
/// infix and prefix functions, with a precedence of NONE
fn getParseRule(kind: TokenType) ParseRule {
    return switch (kind) {
        .LEFT_PAREN => ParseRule.init(Compiler.grouping, null, .NONE),
        .MINUS => ParseRule.init(Compiler.unary, Compiler.binary, .TERM),
        .PLUS => ParseRule.init(null, Compiler.binary, .TERM),
        .SLASH => ParseRule.init(null, Compiler.binary, .FACTOR),
        .STAR => ParseRule.init(null, Compiler.binary, .FACTOR),
        .BANG => ParseRule.init(Compiler.unary, null, .NONE),
        .BANG_EQUAL => ParseRule.init(null, Compiler.binary, .EQUALITY),
        .NUMBER => ParseRule.init(Compiler.number, null, .NONE),
        .TRUE => ParseRule.init(Compiler.literal, null, .NONE),
        .FALSE => ParseRule.init(Compiler.literal, null, .NONE),
        .NIL => ParseRule.init(Compiler.literal, null, .NONE),
        .EQUAL_EQUAL => ParseRule.init(null, Compiler.binary, .COMPARISON),
        .GREATER => ParseRule.init(null, Compiler.binary, .COMPARISON),
        .GREATER_EQUAL => ParseRule.init(null, Compiler.binary, .COMPARISON),
        .LESS => ParseRule.init(null, Compiler.binary, .COMPARISON),
        .LESS_EQUAL => ParseRule.init(null, Compiler.binary, .COMPARISON),
        else => .{},
    };
}
