const std = @import("std");

const log = std.log;
const cwd = std.fs.cwd;

const Allocator = std.mem.Allocator;

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Context = @import("context.zig");

// Global TODOs:
// TODO(main), add linear types

pub fn main() void {
    var dbg = std.heap.DebugAllocator(.{}).init;
    const gpa = dbg.allocator();

    var args = std.process.argsWithAllocator(gpa)
        catch return log.err("Couldn't allocate space for argv", .{});
    defer args.deinit();

    const exec = args.next()
        orelse return log.err("Expected <program> in argv[0]", .{});
    const path = args.next()
        orelse return log.err("Expected <file> in argv[1]", .{});
    _ = exec;

    const source = cwd().readFileAllocOptions(gpa, path, std.math.maxInt(u32), null, .of(u8), 0)
        catch return log.err("Couldn't open '{s}'", .{path});
    defer gpa.free(source);

    compile(gpa, source);
}

fn compile(gpa: Allocator, source: [:0]const u8) void {
    var tokens = Lexer.lex(gpa, source) catch |err| {
        const idx = Lexer.error_idx.?;
        var start = idx;
        var end = idx;

        while (start > 0        and source[start-1] != '\n') start -= 1;
        while (end < source.len and source[end]     != '\n') end += 1;

        std.debug.print("\x1B[31m{}:\x1B[0m\n", .{err});
        std.debug.print("{s}\n", .{source[start..end]});

        for (0..idx-start) |_| std.debug.print(" ", .{});
        std.debug.print("^\n", .{});
        return;
    };

    defer tokens.deinit();
    tokens.debug(source);

    var tree = Parser.parse(gpa, &tokens, source) catch |err| {
        const tdx = Parser.error_idx.?;
        const idx = tokens.at(tdx).idx;
        var start = idx;
        var end = idx;

        while (start > 0        and source[start-1] != '\n') start -= 1;
        while (end < source.len and source[end]     != '\n') end += 1;

        std.debug.print("\x1B[31m{}:\x1B[0m\n", .{err});
        std.debug.print("{s}\n", .{source[start..end]});

        for (0..idx-start) |_| std.debug.print(" ", .{});
        std.debug.print("^\n", .{});
        return;
    };

    defer tree.deinit();
    tree.debug(tokens, source, 0, 0);

    var ctx = Context.scan(gpa, tree, &tokens, source) catch |err| {
        const ndx = Context.error_idx.?;
        const tdx = tree.nodes.items[ndx].main;
        const idx = tokens.at(tdx).idx;
        var start = idx;
        var end = idx;

        while (start > 0        and source[start-1] != '\n') start -= 1;
        while (end < source.len and source[end]     != '\n') end += 1;

        std.debug.print("\x1B[31m{}:\x1B[0m\n", .{err});
        std.debug.print("{s}\n", .{source[start..end]});

        for (0..idx-start) |_| std.debug.print(" ", .{});
        std.debug.print("^\n", .{});
        return;
    };

    defer ctx.deinit();
}
