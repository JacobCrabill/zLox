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
