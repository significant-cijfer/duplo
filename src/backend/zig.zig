const std = @import("std");

const Writer = std.Io.Writer;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const Lexer = @import("../lexer.zig");
const Tokens = Lexer.Tokens;

const Flattener = @import("../flattener.zig");
const Graph = Flattener.Graph;
const Block = Flattener.Block;
const Inst = Flattener.Inst;
const Location = Graph.Location;

pub fn gen(writer: *Writer, graph: Graph, tokens: Tokens) !void {
    for (graph.functions.items) |function| {
        // prologue
        try writer.print("fn {s}() <void> {{\n", .{function.name});

        try genFunctionStates(writer, graph, function.root);
        try genFunctionSwitch(writer, graph, tokens, function.root);

        // epilogue
        try writer.print("}}\n", .{});
    }
}

fn genFunctionStates(writer: *Writer, graph: Graph, root: u32) !void {
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

fn genFunctionSwitch(writer: *Writer, graph: Graph, tokens: Tokens, root: u32) !void {
    const gpa = graph.allocator;

    var visited = AutoHashMap(u32, void).init(gpa);
    defer visited.deinit();

    var queue = ArrayList(u32).empty;
    defer queue.deinit(gpa);

    try queue.append(gpa, root);

    try writer.print("  block: switch (Block.b{}) {{\n", .{root});

    while (queue.pop()) |bdx| {
        const block = graph.blocks.items[bdx];
        const insts = graph.insts.items[block.idx..block.idx+block.len];

        if (visited.contains(bdx)) continue;

        try writer.print("    .b{} => {{\n", .{bdx});

        try genBlockBody(writer, tokens, graph.locations.items, insts);
        try genBlockFlow(writer, block);

        try writer.print("    }},\n", .{});

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

fn genBlockBody(writer: *Writer, tokens: Tokens, locations: []Location, insts: []Inst) !void {
    for (insts) |inst| switch (inst.kind) {
        .set => try genInstSet(writer, tokens, locations, inst),
        else => try writer.print("      // todo <{}>\n", .{inst.kind}),
    };
}

fn genBlockFlow(writer: *Writer, block: Block) !void {
    const flow = block.flow;

    switch (flow.kind) {
        .jmp => try writer.print("      continue :block .b{}\n", .{flow.extra.mono}),
        .jnz => try writer.print("      if ({{{}}}) continue :block .b{} else continue :block .b{}\n", .{flow.extra.cond.chs, flow.extra.cond.lhs, flow.extra.cond.rhs}),
        .ret => try writer.print("      return {{{}}};\n", .{flow.extra.mono}),
    }
}

fn genInstSet(writer: *Writer, tokens: Tokens, locations: []Location, inst: Inst) !void {
    const loc = locations[inst.extra.mon_op.dst];
    const dst = if (loc.main == 0)
        "t?"
    else
        tokens.slice(loc.main);
    const src = tokens.slice(inst.extra.mon_op.src);

    try writer.print("      let {s} = {s};\n", .{dst, src});
}
