const std = @import("std");
const builtin = @import("builtin");

const log = std.log;
const cwd = std.fs.cwd;
const stdout = std.fs.File.stdout();

const Writer = std.Io.Writer;
const Allocator = std.mem.Allocator;

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Context = @import("context.zig");
const Flattener = @import("flattener.zig");
const Generator = @import("generator.zig");

// Global TODOs:
// None :)

pub fn main() void {
    const gpa = switch (builtin.mode) {
        .Debug => b: {
            var dbg = std.heap.DebugAllocator(.{}).init;
            break :b dbg.allocator();
        },
        .ReleaseSafe,
        .ReleaseFast,
        .ReleaseSmall => std.heap.smp_allocator,
    };


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

    var buffer: [8192]u8 = undefined;
    var writer = stdout.writer(&buffer);

    compile(gpa, &writer.interface, source);

    writer.end()
        catch return log.err("Couldn't flush stdout", .{});
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

fn compile(gpa: Allocator, writer: *Writer, source: [:0]const u8) void {
    var timer = std.time.Timer.start()
        catch return scream(error.NoTimer);

    var tokens = Lexer.lex(gpa, source) catch |err| {
        const idx = Lexer.error_idx orelse return scream(err);

        return complain(source, err, idx);
    };

    const lap_tokens = timer.lap();
    defer tokens.deinit();

    var tree = Parser.parse(gpa, &tokens) catch |err| {
        const tdx = Parser.error_idx orelse return scream(err);
        const idx = tokens.at(tdx).idx;

        return complain(source, err, idx);
    };

    const lap_tree = timer.lap();
    defer tree.deinit();

    var tables = Context.scan(gpa, tree, tokens) catch |err| {
        const ndx = Context.error_idx orelse return scream(err);
        const tdx = tree.nodes.items[ndx].main;
        const idx = tokens.at(tdx).idx;

        return complain(source, err, idx);
    };

    const lap_tables = timer.lap();
    defer tables.deinit();

    var graph = Flattener.flatten(gpa, &tables, tree, tokens) catch |err| {
        const ndx = Flattener.error_idx orelse return scream(err);
        const tdx = tree.nodes.items[ndx].main;
        const idx = tokens.at(tdx).idx;

        return complain(source, err, idx);
    };

    const lap_graph = timer.lap();
    defer graph.deinit();

    Generator.generate(.zig, writer, graph, tables, tokens) catch |err| {
        return scream(err);
    };

    const lap_gen = timer.lap();

    std.debug.print("tokens: {}ns\n", .{lap_tokens});
    std.debug.print("tree:   {}ns\n", .{lap_tree});
    std.debug.print("tables: {}ns\n", .{lap_tables});
    std.debug.print("graph:  {}ns\n", .{lap_graph});
    std.debug.print("gen:    {}ns\n", .{lap_gen});
    std.debug.print("total:  {}ns\n", .{lap_tokens + lap_tree + lap_tables + lap_graph + lap_gen});
}
