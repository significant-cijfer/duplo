const std = @import("std");

const cwd = std.fs.cwd;

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");

pub fn main() !void {
    var dbg = std.heap.DebugAllocator(.{}).init;
    const gpa = dbg.allocator();

    var args = try std.process.argsWithAllocator(gpa);
    defer args.deinit();

    const exec = args.next().?;
    const file = args.next() orelse @panic("Expected <file> arg");
    _ = exec;

    const source = try cwd().readFileAllocOptions(gpa, file, std.math.maxInt(u32), null, @alignOf(u8), 0);
    defer gpa.free(source);

    var tokens = try Lexer.lex(gpa, source);
    defer tokens.deinit();

    tokens.debug(source);

    const ast = try Parser.parse(gpa, &tokens, source);
    defer ast.deinit();

    ast.debug(tokens, source, 0, 0);
}
