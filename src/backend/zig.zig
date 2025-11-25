const std = @import("std");

const Writer = std.Io.Writer;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const Flattener = @import("../flattener.zig");
const Graph = Flattener.Graph;

pub fn gen(writer: *Writer, graph: Graph) !void {
    for (graph.functions.items) |function| {
        // prologue
        try writer.print("fn {s}() void {{\n", .{function.name});

        try genFunctionStates(writer, graph, function.root);
        try genFunctionSwitch(writer, graph, function.root);

        // epilogue
        try writer.print("}}\n", .{});
    }
}

pub fn genFunctionStates(writer: *Writer, graph: Graph, root: u32) !void {
    const gpa = graph.allocator;

    var visited = AutoHashMap(u32, void).init(gpa);
    defer visited.deinit();

    var queue = ArrayList(u32).empty;
    defer queue.deinit(gpa);

    try queue.append(gpa, root);

    try writer.print("  const Block = enum {{\n", .{});

    while (queue.pop()) |bdx| {
        if (visited.contains(bdx)) continue;

        try writer.print("    b{},\n", .{bdx});

        const block = graph.blocks.items[bdx];
        switch (block.flow.kind) {
            .jmp => try queue.append(gpa, block.flow.extra.mono),
            .jnz => {
                try queue.append(gpa, block.flow.extra.cond.lhs);
                try queue.append(gpa, block.flow.extra.cond.rhs);
            },
            .ret => {},
        }
    }

    try writer.print("  }};\n", .{});
}

pub fn genFunctionSwitch(writer: *Writer, graph: Graph, root: u32) !void {
    const gpa = graph.allocator;

    var visited = AutoHashMap(u32, void).init(gpa);
    defer visited.deinit();

    var queue = ArrayList(u32).empty;
    defer queue.deinit(gpa);

    try queue.append(gpa, root);

    try writer.print("  switch (Block.b{}) {{\n", .{root});

    while (queue.pop()) |bdx| {
        if (visited.contains(bdx)) continue;

        try writer.print("    .b{} => {{\n", .{bdx});
        try writer.print("    }},\n", .{});

        const block = graph.blocks.items[bdx];
        switch (block.flow.kind) {
            .jmp => try queue.append(gpa, block.flow.extra.mono),
            .jnz => {
                try queue.append(gpa, block.flow.extra.cond.lhs);
                try queue.append(gpa, block.flow.extra.cond.rhs);
            },
            .ret => {},
        }
    }

    try writer.print("  }}\n", .{});
}
