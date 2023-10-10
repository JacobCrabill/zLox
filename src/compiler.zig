const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;

pub fn compile(input: []const u8) !void {
    var lexer = Lexer.init(input);
    while (lexer.scanToken()) |token| {
        std.debug.print("{any}\n", .{token});
        if (token.kind == .EOF) break;
    }
}
