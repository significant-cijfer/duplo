const std = @import("std");

const Writer = std.Io.Writer;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const Lexer = @import("../lexer.zig");
const Tokens = Lexer.Tokens;

const Context = @import("../context.zig");
const Tables = Context.Tables;
const Table = Context.Table;

const Flattener = @import("../flattener.zig");
const Graph = Flattener.Graph;
const Block = Flattener.Block;
const Inst = Flattener.Inst;
const Location = Graph.Location;

const GenLocation = struct {
    locations: []Location,
    tokens: Tokens,
    main: u32,

    pub fn format(
        self: GenLocation,
        writer: *std.Io.Writer,
    ) !void {
        if (self.locations[self.main].main == 0)
            try writer.print("t{}", .{self.main})
        else
            try writer.print("{s}", .{self.tokens.slice(self.locations[self.main].main)});
    }
};

const GenTypx = struct {
    table: Table,
    tokens: Tokens,
    typx: u32,
    name: ?[]const u8, //NOTE, for .function kind

    pub fn format(
        self: GenTypx,
        writer: *std.Io.Writer,
    ) !void {
        const typx = self.table.types.items[self.typx];

        switch (typx.kind) {
            .integer => {
                const sign = if (typx.extra.integer.sign) "i" else "u";
                const bits = typx.extra.integer.bits;

                try writer.print("{s}{}", .{sign, bits});
            },
            .function => {
                try writer.print("fn {s}(", .{self.name orelse ""});

                const proto = typx.extra.function;
                const names = self.table.extra.items[proto.names..proto.names+proto.plen];
                const prms = self.table.extra.items[proto.prms..proto.prms+proto.plen];

                for (names, prms) |name, prm| {
                    const pname = self.tokens.slice(name);
                    const gtypx = GenTypx{ .table = self.table, .tokens = self.tokens, .typx = prm, .name = null };

                    try writer.print("{s}: {f}, ", .{pname, gtypx});
                }

                const gtypx = GenTypx{ .table = self.table, .tokens = self.tokens, .typx = proto.rtyp, .name = null };
                try writer.print(") {f}", .{gtypx});
            },
            else => std.debug.panic("TODO: {}", .{typx.kind}),
        }
    }
};

pub fn gen(writer: *Writer, graph: Graph, tables: Tables, tokens: Tokens) !void {
    for (graph.functions.items) |function| {
        // prologue
        const table = tables.get(function.table).?;
        const symbl = table.get(function.name).?;
        const gtypx = GenTypx{ .table = table, .tokens = tokens, .typx = symbl.typx, .name = function.name };

        //TODO, only print pub when the corresponding attr is set
        try writer.print("pub {f} {{\n", .{gtypx});

        try genFunctionStates(writer, graph, function.root);
        try genFunctionLocals(writer, graph, table, tokens, function.locals);
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

        try visited.put(bdx, {});
    }

    try writer.print("  }};\n", .{});
}

fn genFunctionLocals(writer: *Writer, graph: Graph, table: Table, tokens: Tokens, locals: u32) !void {
    try writer.print("    // TODO, allocate locals\n", .{});

    const len = graph.extra.items[locals];
    const locations = graph.extra.items[locals+1..locals+len+1];

    //const locations: []const u32 = &.{ 0 };
    _ = table;

    for (locations) |main| {
        //const loc = graph.locations.items[main];
        const dst = GenLocation{ .locations = graph.locations.items, .tokens = tokens, .main = main };
        //const gtypx = GenTypx{ .table = table, .tokens = tokens, .typx = loc.typx, .name = null };

        try writer.print("  var {f}: ? = undefined;\n", .{dst});
    }
}

fn genFunctionSwitch(writer: *Writer, graph: Graph, tokens: Tokens, root: u32) !void {
    const gpa = graph.allocator;

    var visited = AutoHashMap(u32, void).init(gpa);
    defer visited.deinit();

    var queue = ArrayList(u32).empty;
    defer queue.deinit(gpa);

    try queue.append(gpa, root);

    try writer.print("  return block: switch (Block.b{}) {{\n", .{root});

    while (queue.pop()) |bdx| {
        const block = graph.blocks.items[bdx];
        const insts = graph.insts.items[block.idx..block.idx+block.len];

        if (visited.contains(bdx)) continue;

        try writer.print("    .b{} => {{\n", .{bdx});

        try genBlockBody(writer, tokens, graph.locations.items, graph.extra.items, insts);
        try genBlockFlow(writer, tokens, graph.locations.items, block);

        try writer.print("    }},\n", .{});

        switch (block.flow.kind) {
            .jmp => try queue.append(gpa, block.flow.extra.mono),
            .jnz => {
                try queue.append(gpa, block.flow.extra.cond.lhs);
                try queue.append(gpa, block.flow.extra.cond.rhs);
            },
            .ret => {},
        }

        try visited.put(bdx, {});
    }

    try writer.print("  }};\n", .{});
}

fn genBlockBody(writer: *Writer, tokens: Tokens, locations: []Location, extras: []u32, insts: []Inst) !void {
    for (insts) |inst| switch (inst.kind) {
        .set => try genInstSet(writer, tokens, locations, inst),
        .store => try genInstStore(writer, tokens, locations, inst),
        .add => try genInstAdd(writer, tokens, locations, inst),
        .sub => try genInstSub(writer, tokens, locations, inst),
        .mul => try genInstMul(writer, tokens, locations, inst),
        .div => try genInstDiv(writer, tokens, locations, inst),
        .call => try genInstCall(writer, tokens, locations, extras, inst),
        else => try writer.print("      // todo <{}>\n", .{inst.kind}),
    };
}

fn genBlockFlow(writer: *Writer, tokens: Tokens, locations: []Location, block: Block) !void {
    const flow = block.flow;

    const main = switch (flow.kind) {
        .jmp => undefined,
        .jnz => flow.extra.cond.chs,
        .ret => flow.extra.mono,
    };

    const gloc = GenLocation{ .locations = locations, .tokens = tokens, .main = main };

    switch (flow.kind) {
        .jmp => try writer.print("      continue :block .b{};\n", .{flow.extra.mono}),
        .jnz => try writer.print("      if ({f} != 0) continue :block .b{} else continue :block .b{};\n", .{gloc, flow.extra.cond.lhs, flow.extra.cond.rhs}),
        .ret => try writer.print("      break :block {f};\n", .{gloc}),
    }
}

fn genInstSet(writer: *Writer, tokens: Tokens, locations: []Location, inst: Inst) !void {
    const dst = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.mon_op.dst };
    const src = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.mon_op.src };

    try writer.print("      const {f} = {f};\n", .{dst, src});
}

fn genInstStore(writer: *Writer, tokens: Tokens, locations: []Location, inst: Inst) !void {
    const dst = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.mon_op.dst };
    const src = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.mon_op.src };

    try writer.print("      {f} = {f};\n", .{dst, src});
}

fn genInstAdd(writer: *Writer, tokens: Tokens, locations: []Location, inst: Inst) !void {
    const dst = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.dst };
    const lhs = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.lhs };
    const rhs = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.rhs };

    try writer.print("      const {f} = {f} + {f};\n", .{dst, lhs, rhs});
}

fn genInstSub(writer: *Writer, tokens: Tokens, locations: []Location, inst: Inst) !void {
    const dst = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.dst };
    const lhs = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.lhs };
    const rhs = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.rhs };

    try writer.print("      const {f} = {f} - {f};\n", .{dst, lhs, rhs});
}

fn genInstMul(writer: *Writer, tokens: Tokens, locations: []Location, inst: Inst) !void {
    const dst = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.dst };
    const lhs = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.lhs };
    const rhs = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.rhs };

    try writer.print("      const {f} = {f} * {f};\n", .{dst, lhs, rhs});
}

fn genInstDiv(writer: *Writer, tokens: Tokens, locations: []Location, inst: Inst) !void {
    const dst = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.dst };
    const lhs = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.lhs };
    const rhs = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.bin_op.rhs };

    try writer.print("      const {f} = {f} / {f};\n", .{dst, lhs, rhs});
}

fn genInstCall(writer: *Writer, tokens: Tokens, locations: []Location, extras: []u32, inst: Inst) !void {
    const dst = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.call.dst };
    const fun = GenLocation{ .locations = locations, .tokens = tokens, .main = inst.extra.call.func };

    const args = extras[inst.extra.call.args..inst.extra.call.args+inst.extra.call.len];

    try writer.print("      const {f} = {f}(", .{dst, fun});

    for (args) |arg| {
        const aloc = GenLocation{ .locations = locations, .tokens = tokens, .main = arg };

        try writer.print("{f}, ", .{aloc});
    }

    try writer.print(");\n", .{});
}
