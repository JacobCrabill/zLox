const std = @import("std");

// zig fmt: off
pub const TokenType = enum(u8) {
    // Single-character tokens
    LEFT_PAREN, RIGHT_PAREN,
    LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS,
    SEMICOLON, SLASH, STAR,

    // One or two character tokens
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // Literals
    IDENTIFIER, STRING, NUMBER,

    // Keywords
    AND, CLASS, ELSE, FALSE,
    FOR, FUN, IF, NIL, OR,
    PRINT, RETURN, SUPER,
    THIS, TRUE, VAR, WHILE,

    ERROR, EOF,
};
// zig fmt: on

pub const Token = struct {
    kind: TokenType = .EOF,
    lexeme: []const u8 = undefined,
    line: usize = 0,
};

pub const Lexer = struct {
    input: []const u8 = undefined,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(input: []const u8) Lexer {
        return .{
            .input = input,
        };
    }

    pub fn scanToken(self: *Lexer) ?Token {
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.EOF);

        //for (self.input) |c| {
        const c: u8 = self.advance();
        return switch (c) {
            '(' => self.makeToken(.LEFT_PAREN),
            ')' => self.makeToken(.RIGHT_PAREN),
            '{' => self.makeToken(.LEFT_BRACE),
            '}' => self.makeToken(.RIGHT_BRACE),
            ';' => self.makeToken(.SEMICOLON),
            ',' => self.makeToken(.COMMA),
            '.' => self.makeToken(.DOT),
            '+' => self.makeToken(.PLUS),
            '-' => self.makeToken(.MINUS),
            '*' => self.makeToken(.STAR),
            '/' => self.makeToken(.SLASH),
            '!' => self.makeToken(if (self.match('=')) blk: {
                break :blk .BANG_EQUAL;
            } else eblk: {
                break :eblk .BANG;
            }),
            '=' => self.makeToken(if (self.match('=')) blk: {
                break :blk .EQUAL_EQUAL;
            } else eblk: {
                break :eblk .EQUAL;
            }),
            '>' => self.makeToken(if (self.match('=')) blk: {
                break :blk .GREATER_EQUAL;
            } else eblk: {
                break :eblk .GREATER;
            }),
            '<' => self.makeToken(if (self.match('=')) blk: {
                break :blk .LESS_EQUAL;
            } else eblk: {
                break :eblk .LESS;
            }),
            else => self.errorToken("Unexpected character"),
        };
    }

    fn isAtEnd(self: Lexer) bool {
        return self.current >= self.input.len;
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.input[self.current - 1];
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.input[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn makeToken(self: Lexer, kind: TokenType) Token {
        return .{
            .kind = kind,
            .lexeme = self.input[self.start..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: Lexer, msg: []const u8) Token {
        return .{
            .kind = .ERROR,
            .lexeme = msg,
            .line = self.line,
        };
    }
};
