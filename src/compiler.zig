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
const VM = @import("vm.zig").VM;

const Lexer = zlox.Lexer;
const Chunk = zlox.Chunk;
const OpCode = zlox.OpCode;
const Token = zlox.Token;
const TokenType = zlox.TokenType;
const Value = zlox.Value;
const Object = zlox.Object;
const ObjType = zlox.ObjType;
const Function = zlox.Function;

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
const ParseFn = *const fn (*Parser, bool) void;

pub const Local = struct {
    name: Token = undefined,
    depth: i16 = 0,
};

pub const FunctionType = enum(u8) {
    Function,
    Script,
};

const LOCALS_MAX: u32 = 256;

pub const Compiler = struct {
    enclosing: ?*Compiler = null,
    fun_obj: *Object = undefined,
    kind: FunctionType = .Script,

    locals: [LOCALS_MAX]Local = undefined,
    local_count: u8 = 0,
    scope_depth: u8 = 0,

    pub fn setup(self: *Compiler, vm: *VM, current: ?*Compiler, kind: FunctionType) !void {
        self.enclosing = current;
        self.fun_obj = try zlox.newFunction(vm);
        self.kind = kind;

        // Initialize the compiler's stack - claim slot 0 for the VM
        self.locals[0].name.lexeme = "";
        self.local_count = 1;
    }
};

pub const Parser = struct {
    const Self = @This();
    alloc: Allocator = undefined,
    vm: *VM = undefined, // necessary for handling Object ownership
    current: Token = undefined,
    previous: Token = undefined,
    lexer: Lexer = undefined,
    hadError: bool = false,
    panicMode: bool = false,
    compiler: Compiler = Compiler{},

    pub fn init(vm: *VM) !Self {
        return .{
            .alloc = vm.alloc,
            .vm = vm,
            // .current = undefined,
            // .previous = undefined,
            // .compiler = try Compiler.init(vm, .Script),
        };
    }

    /// Free all heap allocations
    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn compile(self: *Self, input: []const u8) !*Object {
        try self.compiler.setup(self.vm, null, .Script);
        self.lexer = Lexer.init(input);

        self.hadError = false;
        self.panicMode = false;

        self.advance();

        self.emitConstant(zlox.NoneVal); // HACK until CallFrames implemented

        while (!self.match(.EOF)) {
            self.declaration();
        }

        var fun_obj = self.endCompiler();

        if (self.hadError) return error.CompileError;
        return fun_obj;
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
        self.chunk().writeChunk(byte, self.previous.line) catch {
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

    fn emitJump(self: *Self, op: OpCode) usize {
        self.emitOp(op);
        self.emitByte(0xff);
        self.emitByte(0xff);
        return self.code().len - 2;
    }

    fn emitLoop(self: *Self, loop_start: usize) void {
        self.emitOp(.OP_LOOP);
        const offset = self.code().len - loop_start + 2;
        if (offset >= std.math.maxInt(u16)) {
            self.errorMsg("Loop body is too large!");
        }
        self.emitByte(@intCast((offset >> 8) & 0xff));
        self.emitByte(@intCast(offset & 0xff));
    }

    // Emit two opcodes to the current Chunk being compiled
    fn emitOps(self: *Self, op1: OpCode, op2: OpCode) void {
        self.emitBytes(op1.byte(), op2.byte());
    }

    fn emitReturn(self: *Self) void {
        self.emitOp(.OP_RETURN);
    }

    fn makeConstant(self: *Self, value: Value) u8 {
        const idx = self.chunk().addConstant(value);
        if (idx > std.math.maxInt(u8)) {
            self.errorMsg("Too many constants in one chunk!");
            return 0;
        }

        return idx;
    }

    fn emitConstant(self: *Self, value: Value) void {
        self.emitBytes(OpCode.OP_CONSTANT.byte(), self.makeConstant(value));
    }

    fn patchJump(self: *Self, offset: usize) void {
        const jump: usize = self.code().len - offset - 2;
        if (jump >= std.math.maxInt(u16)) {
            self.errorMsg("Jump size is too large!");
        }

        self.code()[offset] = @enumFromInt((jump >> 8) & 0xff);
        self.code()[offset + 1] = @enumFromInt(jump & 0xff);
    }

    fn endCompiler(self: *Self) *Object {
        self.emitReturn();
        var fun_obj: *Object = self.compiler.fun_obj;

        if (builtin.mode == .Debug and !self.hadError) {
            std.debug.assert(fun_obj.* == ObjType.function);
            if (fun_obj.function.name.len > 0) {
                zlox.disassembleChunk(self.chunk(), fun_obj.function.name);
            } else {
                zlox.disassembleChunk(self.chunk(), "<script>");
            }
        }

        return fun_obj;
    }

    fn beginScope(self: *Self) void {
        self.compiler.scope_depth += 1;
    }

    fn endScope(self: *Self) void {
        self.compiler.scope_depth -= 1;
        std.debug.print("Ending current scope\n", .{});

        while (self.compiler.local_count > 0 and
            self.compiler.locals[self.compiler.local_count - 1].depth > self.compiler.scope_depth)
        {
            self.emitOp(.OP_POP);
            self.compiler.local_count -= 1;
        }
    }

    fn binary(self: *Self, can_assign: bool) void {
        _ = can_assign;
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

    fn literal(self: *Self, can_assign: bool) void {
        _ = can_assign;
        switch (self.previous.kind) {
            .TRUE => self.emitOp(.OP_TRUE),
            .FALSE => self.emitOp(.OP_FALSE),
            .NIL => self.emitOp(.OP_NIL),
            else => {},
        }
    }

    pub fn grouping(self: *Self, can_assign: bool) void {
        _ = can_assign;
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after expression");
    }

    pub fn number(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const value: f64 = std.fmt.parseFloat(f64, self.previous.lexeme) catch 0;
        self.emitConstant(Value{ .number = value });
    }

    pub fn string(self: *Self, can_assign: bool) void {
        _ = can_assign;
        // Copy string out of input buffer to a buffer owned by the Object
        const raw_str = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        var obj_str = zlox.copyString(self.vm, raw_str) catch {
            self.errorMsg("Unable to copy string");
            return;
        };
        self.emitConstant(Value{ .object = obj_str });
    }

    fn namedVariable(self: *Self, token: Token, can_assign: bool) void {
        var get_op: OpCode = .OP_GET_LOCAL;
        var set_op: OpCode = .OP_SET_LOCAL;
        var arg: i16 = self.resolveLocal(token);

        if (arg < 0) {
            get_op = .OP_GET_GLOBAL;
            set_op = .OP_SET_GLOBAL;
            arg = self.identifierConstant(token);
        }

        if (can_assign and self.match(.EQUAL)) {
            self.expression();
            std.debug.print("{}\n", .{set_op});
            self.emitBytes(set_op.byte(), @intCast(arg));
        } else {
            std.debug.print("{}\n", .{get_op});
            self.emitBytes(get_op.byte(), @intCast(arg));
        }
    }

    fn variable(self: *Self, can_assign: bool) void {
        self.namedVariable(self.previous, can_assign);
    }

    fn unary(self: *Self, can_assign: bool) void {
        _ = can_assign;
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

        const can_assign: bool = @intFromEnum(prec) <= @intFromEnum(Precedence.ASSIGNMENT);

        if (getParseRule(self.previous.kind).prefix) |*prefixRule| {
            prefixRule.*(self, can_assign);
        } else {
            self.errorMsg("Invalid expression");
            return;
        }

        while (@intFromEnum(prec) <= @intFromEnum(getParseRule(self.current.kind).precedence)) {
            self.advance();
            if (getParseRule(self.previous.kind).infix) |infixRule| {
                infixRule(self, can_assign);
            }
        }

        if (can_assign and self.match(.EQUAL)) {
            self.errorMsg("Invalid assignment target");
        }
    }

    fn identifierConstant(self: *Self, token: Token) u8 {
        var obj_str: *Object = zlox.copyString(self.vm, token.lexeme) catch {
            self.errorMsg("Unable to copy string");
            return 0;
        };
        return self.makeConstant(.{ .object = obj_str });
    }

    fn resolveLocal(self: *Self, name: Token) i16 {
        var i: i16 = @as(i16, self.compiler.local_count) - 1;
        while (i >= 0) : (i -= 1) {
            const idx: usize = @intCast(i);
            const local = self.compiler.locals[idx];
            if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                if (local.depth == -1) {
                    self.errorMsg("Can't read local variable in its own initializer");
                }
                return i;
            }
        }

        return -1;
    }

    fn addLocal(self: *Self, token: Token) void {
        if (self.compiler.local_count >= std.math.maxInt(u8)) {
            self.errorMsg("Too many local variables in function");
            return;
        }

        var local: *Local = &self.compiler.locals[self.compiler.local_count];
        local.name = token;
        local.depth = -1;
        self.compiler.local_count += 1;
    }

    fn declareVariable(self: *Self) void {
        if (self.compiler.scope_depth == 0) return; // Globals are late-bound

        const name: Token = self.previous;
        var i: i16 = @as(i16, self.compiler.local_count) - 1;
        while (i >= 0) : (i -= 1) {
            var local: *Local = &self.compiler.locals[@intCast(i)];
            if (local.depth != -1 and local.depth < self.compiler.scope_depth) {
                break;
            }

            if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                self.errorMsg("Duplicate variable declaration");
            }
        }

        self.addLocal(self.previous);
    }

    fn parseVariable(self: *Self, error_msg: []const u8) u8 {
        self.consume(.IDENTIFIER, error_msg);

        self.declareVariable();
        if (self.compiler.scope_depth > 0) return 0; // Locals aren't resolved at runtime

        return self.identifierConstant(self.previous);
    }

    fn defineVariable(self: *Self, global: u8) void {
        if (self.compiler.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        self.emitBytes(OpCode.OP_DEFINE_GLOBAL.byte(), global);
    }

    fn markInitialized(self: *Self) void {
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    /// Skip the remaining condition expression if the preceding expression was falsey
    fn _and(self: *Self, can_assign: bool) void {
        _ = can_assign;
        const end_jump: usize = self.emitJump(.OP_JUMP_IF_FALSE);

        self.emitOp(.OP_POP);
        self.parsePrecedence(.AND);

        self.patchJump(end_jump);
    }

    /// Skip the remaining condition expression if the preceding expression was truthy
    fn _or(self: *Self, can_assign: bool) void {
        _ = can_assign;

        const else_jump: usize = self.emitJump(.OP_JUMP_IF_FALSE);
        const end_jump: usize = self.emitJump(.OP_JUMP);

        self.patchJump(else_jump);
        self.emitOp(.OP_POP);

        self.parsePrecedence(.OR);
        self.patchJump(end_jump);
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(.ASSIGNMENT);
    }

    fn block(self: *Self) void {
        std.debug.print("Begin block\n", .{});
        while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
            std.debug.print("in block\n", .{});
            self.declaration();
        }

        self.consume(.RIGHT_BRACE, "Missing '}' after block");
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
        } else if (self.match(.LEFT_BRACE)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else if (self.match(.IF)) {
            self.ifStatement();
        } else if (self.match(.WHILE)) {
            self.whileStatement();
        } else if (self.match(.FOR)) {
            self.forStatement();
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
        std.debug.print("expr statement\n", .{});
        self.emitOp(.OP_POP);
    }

    fn ifStatement(self: *Self) void {
        self.consume(.LEFT_PAREN, "Expected '(' after 'if'");
        self.expression();
        self.consume(.RIGHT_PAREN, "Expected '(' after condition");

        const then_jump: usize = self.emitJump(.OP_JUMP_IF_FALSE);
        self.emitOp(.OP_POP); // Pop the condition off the stack
        if (self.match(.LEFT_BRACE)) {
            self.block();
        } else {
            self.statement();
        }

        const else_jump: usize = self.emitJump(.OP_JUMP);

        self.patchJump(then_jump);
        self.emitOp(.OP_POP); // Pop the condition off the stack

        if (self.match(.ELSE)) {
            if (self.match(.LEFT_BRACE)) {
                self.block();
            } else {
                self.statement();
            }
        }
        self.patchJump(else_jump);
    }

    fn whileStatement(self: *Self) void {
        const loop_start = self.code().len;

        self.consume(.LEFT_PAREN, "Expected '(' after 'while'");
        self.expression();
        self.consume(.RIGHT_PAREN, "Expected '(' after condition");

        const exit_jump: usize = self.emitJump(.OP_JUMP_IF_FALSE);
        self.emitOp(.OP_POP); // Pop the condition off the stack
        if (self.match(.LEFT_BRACE)) {
            self.block();
        } else {
            self.statement();
        }
        self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        self.emitOp(.OP_POP);
    }

    fn forStatement(self: *Self) void {
        self.beginScope();
        self.consume(.LEFT_PAREN, "Expected '(' after 'while'");

        if (self.match(.SEMICOLON)) {
            // no initializer
        } else if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loop_start = self.code().len;
        var exit_jump: usize = 0;
        var has_condition: bool = false;
        if (!self.match(.SEMICOLON)) {
            self.expression();
            self.consume(.SEMICOLON, "Expect ';' after loop condition");

            exit_jump = self.emitJump(.OP_JUMP_IF_FALSE);
            has_condition = true;
            self.emitOp(.OP_POP); // Condition
        }

        if (!self.match(.RIGHT_PAREN)) {
            const body_jump = self.emitJump(.OP_JUMP);
            const increment_start = self.code().len;

            self.expression();
            self.emitOp(.OP_POP);

            self.consume(.RIGHT_PAREN, "Expected '(' after 'for' clauses");

            self.emitLoop(loop_start);
            loop_start = increment_start;
            self.patchJump(body_jump);
        }

        if (self.match(.LEFT_BRACE)) {
            self.block();
        } else {
            self.statement();
        }
        self.emitLoop(loop_start);

        // Check if we had a loop condition expression
        if (has_condition) {
            self.patchJump(exit_jump);
            self.emitOp(.OP_POP);
        }
        self.endScope();
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

    /// Get the current chunk being compiled into
    fn chunk(self: *Self) *Chunk {
        return &self.compiler.fun_obj.function.chunk;
    }

    /// Get the current bytecode slice
    fn code(self: *Self) []OpCode {
        return self.chunk().code.items;
    }

    // Error Handling Utility Functions

    fn errorAtCurrent(self: *Self, msg: []const u8) void {
        self.errorAt(&self.current, msg);
    }

    fn errorAt(self: *Self, token: *const Token, msg: []const u8) void {
        if (self.panicMode) return; // don't keep reporting new errors
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
        .LEFT_PAREN => ParseRule.init(Parser.grouping, null, .NONE),
        .MINUS => ParseRule.init(Parser.unary, Parser.binary, .TERM),
        .PLUS => ParseRule.init(null, Parser.binary, .TERM),
        .SLASH => ParseRule.init(null, Parser.binary, .FACTOR),
        .STAR => ParseRule.init(null, Parser.binary, .FACTOR),
        .BANG => ParseRule.init(Parser.unary, null, .NONE),
        .BANG_EQUAL => ParseRule.init(null, Parser.binary, .EQUALITY),
        .STRING => ParseRule.init(Parser.string, null, .NONE),
        .NUMBER => ParseRule.init(Parser.number, null, .NONE),
        .AND => ParseRule.init(null, Parser._and, .AND),
        .OR => ParseRule.init(null, Parser._or, .OR),
        .TRUE => ParseRule.init(Parser.literal, null, .NONE),
        .FALSE => ParseRule.init(Parser.literal, null, .NONE),
        .NIL => ParseRule.init(Parser.literal, null, .NONE),
        .EQUAL_EQUAL => ParseRule.init(null, Parser.binary, .COMPARISON),
        .GREATER => ParseRule.init(null, Parser.binary, .COMPARISON),
        .GREATER_EQUAL => ParseRule.init(null, Parser.binary, .COMPARISON),
        .LESS => ParseRule.init(null, Parser.binary, .COMPARISON),
        .LESS_EQUAL => ParseRule.init(null, Parser.binary, .COMPARISON),
        .IDENTIFIER => ParseRule.init(Parser.variable, null, .NONE),
        else => .{},
    };
}
