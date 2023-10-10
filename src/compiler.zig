const Lexer = @import("lexer.zig").Lexer;

pub fn compile(input: []const u8) !void {
    var lexer = Lexer.init(input);
    while (lexer.scanToken()) |token| {
        _ = token;
    }
}
