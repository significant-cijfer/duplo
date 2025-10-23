const std = @import("std");

const log = std.log;
const cwd = std.fs.cwd;

const Allocator = std.mem.Allocator;

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Context = @import("context.zig");
const Dependency = @import("dependency.zig");

// Global TODOs:
// None :)

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

fn complain(source: [:0]const u8, err: anyerror, idx: u32) void {
    var start = idx;
    var end = idx;

    while (start > 0        and source[start-1] != '\n') start -= 1;
    while (end < source.len and source[end]     != '\n') end += 1;

    std.debug.print("\x1B[31m{}:\x1B[0m\n", .{err});
    std.debug.print("{s}\n", .{source[start..end]});

    for (0..idx-start) |_| std.debug.print(" ", .{});
    std.debug.print("^\n", .{});
}

fn scream(err: anyerror) void {
    std.debug.print("\x1B[31mUnhandleable {}\x1B[0m\n", .{err});
}

fn compile(gpa: Allocator, source: [:0]const u8) void {
    var tokens = Lexer.lex(gpa, source) catch |err| {
        const idx = Lexer.error_idx orelse return scream(err);

        return complain(source, err, idx);
    };

    defer tokens.deinit();
    tokens.debug(source);

    var tree = Parser.parse(gpa, &tokens, source) catch |err| {
        const tdx = Parser.error_idx orelse return scream(err);
        const idx = tokens.at(tdx).idx;

        return complain(source, err, idx);
    };

    defer tree.deinit();
    tree.debug(tokens, source, 0, 0);

    var ctx = Context.scan(gpa, tree, tokens, source) catch |err| {
        const ndx = Context.error_idx orelse return scream(err);
        const tdx = tree.nodes.items[ndx].main;
        const idx = tokens.at(tdx).idx;

        return complain(source, err, idx);
    };

    defer ctx.deinit();

    var graph = Dependency.construct(gpa, tree) catch |err| {
        const ndx = Dependency.error_idx orelse return scream(err);
        const tdx = tree.nodes.items[ndx].main;
        const idx = tokens.at(tdx).idx;

        return complain(source, err, idx);
    };

    defer graph.deinit();
}
