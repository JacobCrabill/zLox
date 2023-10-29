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

const Lexer = zlox.Lexer;
const Chunk = zlox.Chunk;
const OpCode = zlox.OpCode;
const Token = zlox.Token;
const TokenType = zlox.TokenType;
const Value = zlox.Value;
const Object = zlox.Object;

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
    strings: std.BufSet,

    pub fn init(alloc: Allocator) Self {
        return .{
            .alloc = alloc,
            .current = undefined,
            .previous = undefined,
            .strings = std.BufSet.init(alloc),
        };
    }

    /// Free all heap allocations
    pub fn deinit(self: *Self) void {
        self.strings.deinit();
    }

    pub fn compile(self: *Self, input: []const u8, chunk: *Chunk) !void {
        self.lexer = Lexer.init(input);

        self.chunk = chunk;
        self.hadError = false;
        self.panicMode = false;

        self.advance();

        while (!self.match(.EOF)) {
            self.declaration();
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

    fn check(self: Self, kind: TokenType) bool {
        return self.current.kind == kind;
    }

    fn match(self: *Self, kind: TokenType) bool {
        if (!self.check(kind)) return false;
        self.advance();
        return true;
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

    // Emit a single opcode to the current Chunk being compiled
    fn emitOp(self: *Self, op: OpCode) void {
        self.emitByte(op.byte());
    }

    // Emit two opcodes to the current Chunk being compiled
    fn emitOps(self: *Self, op1: OpCode, op2: OpCode) void {
        self.emitBytes(op1.byte(), op2.byte());
    }

    fn emitReturn(self: *Self) void {
        self.emitOp(.OP_RETURN);
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
            .PLUS => self.emitOp(.OP_ADD),
            .MINUS => self.emitOp(.OP_SUBTRACT),
            .STAR => self.emitOp(.OP_MULTIPLY),
            .SLASH => self.emitOp(.OP_DIVIDE),
            .BANG_EQUAL => self.emitOps(.OP_EQUAL, .OP_NOT),
            .EQUAL_EQUAL => self.emitOp(.OP_EQUAL),
            .GREATER => self.emitOp(.OP_GREATER),
            .GREATER_EQUAL => self.emitOps(.OP_LESS, .OP_NOT),
            .LESS => self.emitOp(.OP_LESS),
            .LESS_EQUAL => self.emitOps(.OP_GREATER, .OP_NOT),
            else => return,
        }
    }

    fn literal(self: *Self) void {
        switch (self.previous.kind) {
            .TRUE => self.emitOp(.OP_TRUE),
            .FALSE => self.emitOp(.OP_FALSE),
            .NIL => self.emitOp(.OP_NIL),
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

    pub fn string(self: *Self) void {
        // Copy string out of input buffer to a buffer owned by the Object
        const raw_str = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        self.strings.insert(raw_str) catch {
            self.errorMsg("Unable to copy string");
            return;
        };
        // Get a pointer to the self-owned copy of the string
        const str: []const u8 = self.strings.hash_map.getKey(raw_str).?;
        self.emitConstant(Value{ .object = .{ .string = str } });
    }

    fn unary(self: *Self) void {
        const optype: TokenType = self.previous.kind;

        self.parsePrecedence(.UNARY);

        switch (optype) {
            .MINUS => self.emitOp(.OP_NEGATE),
            .BANG => self.emitOp(.OP_NOT),
            else => return,
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

    fn identifierConstant(self: *Self, token: Token) u8 {
        // copy the identifier string into our strings set
        self.strings.insert(token.lexeme) catch |err| {
            std.debug.print("{any}\n", .{err});
            return 0;
        };
        // Use the copied slice for the Object.String, not the input string
        const ident: []const u8 = self.strings.hash_map.getKey(token.lexeme).?;
        return self.makeConstant(.{ .object = .{ .string = ident } });
    }

    fn parseVariable(self: *Self, error_msg: []const u8) u8 {
        self.consume(.IDENTIFIER, error_msg);
        return self.identifierConstant(self.previous);
    }

    fn defineVariable(self: *Self, global: u8) void {
        self.emitBytes(OpCode.OP_DEFINE_GLOBAL.byte(), global);
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.ASSIGNMENT);
    }

    fn declaration(self: *Self) void {
        if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.panicMode) self.synchronize();
    }

    fn varDeclaration(self: *Self) void {
        const global: u8 = self.parseVariable("Expected variable name");

        if (self.match(.EQUAL)) {
            self.expression();
        } else {
            self.emitOp(.OP_NIL);
        }
        self.consume(.SEMICOLON, "Expected ';' after variable declaration");

        self.defineVariable(global);
    }

    fn statement(self: *Self) void {
        if (self.match(.PRINT)) {
            self.printStatement();
        } else {
            self.expressionStatement();
        }
    }

    fn printStatement(self: *Self) void {
        self.expression();
        self.consume(.SEMICOLON, "Expected ';' after statement");
        self.emitOp(.OP_PRINT);
    }

    fn expressionStatement(self: *Self) void {
        self.expression();
        self.consume(.SEMICOLON, "Expected ';' after expression");
        self.emitOp(.OP_POP);
    }

    fn synchronize(self: *Self) void {
        self.panicMode = false;
        while (self.current.kind != .EOF) {
            if (self.previous.kind == .SEMICOLON) return;
            switch (self.current.kind) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {},
            }

            self.advance();
        }
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
        .STRING => ParseRule.init(Compiler.string, null, .NONE),
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
