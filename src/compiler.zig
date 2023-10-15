const std = @import("std");

const zlox = struct {
    usingnamespace @import("lexer.zig");
    usingnamespace @import("chunk.zig");
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
    }

    fn binary(self: *Self) void {
        const optype: TokenType = self.previous.kind;
        var rule: ParseRule = self.getRule(optype);
        self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (optype) {
            .PLUS => self.emitByte(OpCode.OP_ADD.byte()),
            .MINUS => self.emitByte(OpCode.OP_SUBTRACT.byte()),
            .STAR => self.emitByte(OpCode.OP_MULTIPLY.byte()),
            .SLASH => self.emitByte(OpCode.OP_DIVIDE.byte()),
            else => return,
        }
    }

    pub fn grouping(self: *Self) void {
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after expression");
    }

    pub fn number(self: *Self) void {
        const value: f64 = std.fmt.parseFloat(f64, self.previous.lexeme) catch 0;
        self.emitConstant(Value{ .value = value });
    }

    fn unary(self: *Self) void {
        const optype: TokenType = self.previous.kind;

        self.parsePrecedence(.UNARY);

        switch (optype) {
            .MINUS => self.emitByte(OpCode.OP_NEGATE.byte()),
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

    fn getRule(self: *Self, kind: TokenType) ParseRule {
        _ = self;
        return switch (kind) {
            .LEFT_PAREN => ParseRule.init(Compiler.grouping, null, .NONE),
            .MINUS => ParseRule.init(Compiler.unary, Compiler.binary, .TERM),
            .PLUS => ParseRule.init(null, Compiler.binary, .TERM),
            .SLASH => ParseRule.init(null, Compiler.binary, .FACTOR),
            .STAR => ParseRule.init(null, Compiler.binary, .FACTOR),
            .NUMBER => ParseRule.init(Compiler.number, null, .NONE),
            else => .{},
        };
        // .MINUS => .{ .prefix = self.unary, .infix = self.binary, .precedence = .TERM },
        // .PLUS => .{ .prefix = null, .infix = self.binary, .precedence = .TERM },
        // .SLASH => .{ .prefix = null, .infix = self.binary, .precedence = .FACTOR },
        // .STAR => .{ .prefix = null, .infix = self.binary, .precedence = .FACTOR },
        // .NUMBER => .{ .prefix = self.number, .infix = null, .precedence = .NONE },

    }

    fn parsePrecedence(self: *Self, prec: Precedence) void {
        self.advance();
        if (self.getRule(self.previous.kind).prefix) |*prefixRule| {
            prefixRule.*(self);
        } else {
            self.errorMsg("Expected expression");
            return;
        }
        _ = prec;
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
