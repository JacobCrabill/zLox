const std = @import("std");

const Token = @import("tokens.zig").Token;
const TokenType = @import("tokens.zig").TokenType;

/// Check if a character is a numeric digit
fn isDigit(c: u8) bool {
    return switch (c) {
        '0'...'9' => true,
        else => false,
    };
}

/// Check if a character is an alphabetic letter (or '_')
fn isAlpha(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}

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
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.EOF);

        const c: u8 = self.advance();
        return switch (c) {
            0 => self.makeToken(.EOF),
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
            '"' => self.string(),
            '0'...'9' => self.number(),
            'a'...'z', 'A'...'Z', '_' => self.identifier(),
            else => blk: {
                std.debug.print("Unexpected char: 0x{x}\n", .{c});
                break :blk self.errorToken("Unexpected character");
            },
        };
    }

    fn isAtEnd(self: Lexer) bool {
        return self.current >= self.input.len;
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.input[self.current - 1];
    }

    fn peek(self: Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.input[self.current];
    }

    fn peekNext(self: Lexer) u8 {
        if (self.current >= self.input.len - 1) return 0;
        return self.input[self.current + 1];
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

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    // Skip over comments
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                        // don't return; loop around to handle the '\n'
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn checkKeyword(self: Lexer, offset: usize, len: usize, rest: []const u8, kind: TokenType) TokenType {
        const start = self.start + offset;
        const end = self.start + offset + len;
        if (self.current - self.start == offset + len and std.mem.eql(u8, self.input[start..end], rest)) {
            return kind;
        }

        return .IDENTIFIER;
    }

    fn identifierType(self: Lexer) TokenType {
        return switch (self.input[self.start]) {
            'a' => self.checkKeyword(1, 2, "nd", TokenType.AND),
            'c' => self.checkKeyword(1, 4, "lass", TokenType.CLASS),
            'e' => self.checkKeyword(1, 3, "lse", TokenType.ELSE),
            'f' => blk: {
                if (self.current - self.start > 1) {
                    switch (self.input[self.start + 1]) {
                        'a' => break :blk self.checkKeyword(2, 3, "lse", TokenType.FALSE),
                        'o' => break :blk self.checkKeyword(2, 1, "r", TokenType.FOR),
                        'u' => break :blk self.checkKeyword(2, 1, "n", TokenType.FUN),
                        else => break :blk .IDENTIFIER,
                    }
                }
                break :blk .IDENTIFIER;
            },
            'i' => self.checkKeyword(1, 1, "f", TokenType.IF),
            'n' => self.checkKeyword(1, 2, "il", TokenType.NIL),
            'o' => self.checkKeyword(1, 1, "r", TokenType.OR),
            'p' => self.checkKeyword(1, 4, "rint", TokenType.PRINT),
            'r' => self.checkKeyword(1, 5, "eturn", TokenType.RETURN),
            's' => self.checkKeyword(1, 4, "uper", TokenType.SUPER),
            't' => blk: {
                if (self.current - self.start > 1) {
                    switch (self.input[self.start + 1]) {
                        'h' => break :blk self.checkKeyword(2, 2, "is", TokenType.THIS),
                        'r' => break :blk self.checkKeyword(2, 2, "ue", TokenType.TRUE),
                        else => break :blk .IDENTIFIER,
                    }
                }
                break :blk .IDENTIFIER;
            },
            'v' => self.checkKeyword(1, 2, "ar", TokenType.VAR),
            'w' => self.checkKeyword(1, 4, "hile", TokenType.WHILE),
            else => .IDENTIFIER,
        };
    }

    fn string(self: *Lexer) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string");
        }

        _ = self.advance(); // The closing quote

        return self.makeToken(.STRING);
    }

    fn number(self: *Lexer) Token {
        while (isDigit(self.peek())) _ = self.advance();

        // Look for a decimal portion
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance(); // Consume the '.'

            while (isDigit(self.peek())) _ = self.advance();
        }

        return self.makeToken(.NUMBER);
    }

    fn identifier(self: *Lexer) Token {
        while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
            _ = self.advance();
        }
        return self.makeToken(self.identifierType());
    }
};

test "lexer basics" {
    const input = "if for true false while and"; // TODO: finish...
    var lexer = Lexer.init(input);

    const expected_tokens = [_]TokenType{ .IF, .FOR, .TRUE, .FALSE, .WHILE, .AND, .EOF };

    var idx: usize = 0;
    while (lexer.scanToken()) |token| {
        try std.testing.expectEqual(expected_tokens[idx], token.kind);
        idx += 1;
        if (token.kind == .EOF) break;
    }
    try std.testing.expectEqual(expected_tokens.len, idx);
}
