const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lexer = @import("lexer.zig");
const Tokens = Lexer.Tokens;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

const Context = @import("context.zig");
const Tables = Context.Tables;
const Typx = Context.Typx;

pub var error_idx: ?u32 = null;

const Error = error {
    UnhandledFlatten,
}
    || Allocator.Error;

pub const Graph = struct {
    allocator: Allocator,
    functions: ArrayList(Function),
    locations: ArrayList(Location),
    blocks: ArrayList(Block),
    insts: ArrayList(Inst),
    scope: Scope,

    const Location = struct {
        typx: Typx,
    };

    const Scope = enum {
        root,
        function,
    };

    //NOTE, its not possible to destructure structs with named fields
    //      so we define a tuple here
    const Flat = struct {
        u32, //block
        u32, //location
    };

    pub fn deinit(self: *Graph) void {
        self.blocks.deinit(self.allocator);
        self.insts.deinit(self.allocator);
    }

    fn reserveBlock(self: *Graph) !u32 {
        const idx = self.blocks.items.len;
        try self.blocks.append(self.allocator, .{
            .idx = @intCast(self.insts.items.len),
            .len = undefined,
            .flow = undefined,
        });
        return @intCast(idx);
    }

    fn reserveLocation(self: *Graph, typx: Typx) !u32 {
        const idx = self.locations.items.len;
        try self.locations.append(self.allocator, .{
            .typx = typx,
        });
        return @intCast(idx);
    }

    fn flatten(self: *Graph, tables: Tables, tree: Ast, tokens: Tokens, tdx: u32, bdx: u32, idx: u32) !Flat {
        const table = tables.get(tdx).?;
        const node = tree.nodes.items[idx];
        var block = bdx;

        errdefer { if (error_idx == null) error_idx = idx; }

        switch (node.kind) {
            .root => {
                const roots = tree.extras(node.extra);

                for (roots) |root| {
                    //NOTE, block reassignment is technically useless here
                    //      but it feels nice to do it anyway
                    block, _ = try self.flatten(tables, tree, tokens, idx, block, root);
                }

                return .{ block, try self.reserveLocation(.VOID) };
            },
            .fdecl => {
                const name = tokens.slice(node.main+1);
                const root = try self.reserveBlock();

                self.scope = .function;
                defer self.scope = .root;

                try self.functions.append(self.allocator, .{
                    .name = name,
                    .root = root,
                });

                return try self.flatten(tables, tree, tokens, idx, root, node.extra.fdecl.body);
            },
            .integer => {
                const dst = try self.reserveLocation(.INTEGER);

                try self.insts.append(self.allocator, .{
                    .kind = .set,
                    .extra = .{ .mon_op = .{
                        .dst = dst,
                        .src = node.main
                    }},
                });

                return .{ block, dst };
            },
            .identifier => {
                const name = tokens.slice(node.main);
                const symb = table.get(name).?;
                const typx = table.types.items[symb.typx];

                const dst = try self.reserveLocation(typx);

                try self.insts.append(self.allocator, .{
                    .kind = .load,
                    .extra = .{ .mon_op = .{
                        .dst = dst,
                        .src = node.main
                    }},
                });

                return .{ block, dst };
            },
            .vardef => {
                if (self.scope == .root) return .{ block, try self.reserveLocation(.VOID) };

                const name = tokens.slice(node.main+1);
                const symb = table.get(name).?;
                const typx = table.types.items[symb.typx];

                block, const src = try self.flatten(tables, tree, tokens, tdx, block, node.extra.bin_op.rhs);
                const dst = try self.reserveLocation(typx);

                try self.insts.append(self.allocator, .{
                    .kind = .store,
                    .extra = .{ .mon_op = .{
                        .dst = dst,
                        .src = src,
                    }},
                });

                return .{ block, dst };
            },
            .block => {
                const stmts = tree.extras(node.extra);

                for (stmts) |stmt| {
                    block, _ = try self.flatten(tables, tree, tokens, idx, block, stmt);
                }

                return .{ block, try self.reserveLocation(.VOID) };
            },
            .add, .sub, .mul, .div => {
                block, const lhs = try self.flatten(tables, tree, tokens, tdx, block, node.extra.bin_op.lhs);
                block, const rhs = try self.flatten(tables, tree, tokens, tdx, block, node.extra.bin_op.rhs);

                const loct = self.locations.items[lhs];
                const dst = try self.reserveLocation(loct.typx);

                const kind: Inst.Kind = switch (node.kind) {
                    .add => .add,
                    .sub => .sub,
                    .mul => .mul,
                    .div => .div,
                    else => unreachable,
                };

                try self.insts.append(self.allocator, .{
                    .kind = kind,
                    .extra = .{ .bin_op = .{
                        .dst = dst,
                        .lhs = lhs,
                        .rhs = rhs,
                    }},
                });

                return .{ block, dst };
            },
            .ret => {
                block, const src = try self.flatten(tables, tree, tokens, tdx, block, node.extra.mon_op);
                const dst = try self.reserveLocation(.NORETURN);

                const rdx: u32 = self.blocks.items[bdx].idx;
                const len: u32 = @intCast(self.insts.items.len);

                self.blocks.items[bdx] = .{
                    .idx = rdx,
                    .len = len - rdx,
                    .flow = .{
                        .kind = .ret,
                        .extra = .{ .mono = src },
                    },
                };

                return .{ block, dst };
            },
            else => return error.UnhandledFlatten,
        }
    }

    pub fn debug(self: Graph, tokens: Tokens) void {
        for (self.functions.items) |function| {
            std.log.info("Function: {s}", .{function.name});

            const block = self.blocks.items[function.root];
            for (self.insts.items[block.idx..block.idx+block.len]) |inst| switch (inst.kind) {
                .set => std.log.info("  {any}:   {{{}}} = {s}", .{
                    inst.kind,
                    inst.extra.mon_op.dst,
                    tokens.slice(inst.extra.mon_op.src)
                }),
                .load => std.log.info("  {any}:  {{{}}} = {s}", .{
                    inst.kind,
                    inst.extra.mon_op.dst,
                    tokens.slice(inst.extra.mon_op.src)
                }),
                .store => std.log.info("  {any}: {{{}}} = {{{}}}", .{
                    inst.kind,
                    inst.extra.mon_op.dst,
                    inst.extra.mon_op.src,
                }),
                .add, .sub, .mul, .div => std.log.info("  {any}:   {{{}}} = {{{}}} + {{{}}}", .{
                    inst.kind,
                    inst.extra.bin_op.dst,
                    inst.extra.bin_op.lhs,
                    inst.extra.bin_op.rhs,
                }),
            };

            switch (block.flow.kind) {
                .ret => std.log.info("  {any}:   {{{}}}", .{
                    block.flow.kind,
                    block.flow.extra.mono,
                }),
                else => @panic("TODO"),
            }
        }

        std.log.info("Locations:", .{});
        for (self.locations.items, 0..) |location, idx| {
            std.log.info("  {{{}}} : {any}", .{idx, location.typx.kind});
        }
    }
};

const Function = struct {
    name: []const u8,
    root: u32,
};

pub const Block = struct {
    idx: u32,
    len: u32,
    flow: Flow,
};

const Flow = struct {
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        jmp,
        jnz,
        ret,
    };

    const Extra = union {
        mono: u32,
        cond: Cond,

        const Cond = struct {
            chs: u32,
            lhs: u32,
            rhs: u32,
        };
    };
};

pub const Inst = struct {
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        set,
        load,
        store,
        add,
        sub,
        mul,
        div,
    };

    const Extra = union {
        mon_op: MonOp,
        bin_op: BinOp,

        const MonOp = struct {
            dst: u32,
            src: u32,
        };

        const BinOp = struct {
            dst: u32,
            lhs: u32,
            rhs: u32,
        };
    };
};

pub fn flatten(gpa: Allocator, tables: Tables, tree: Ast, tokens: Tokens) !Graph {
    var graph = Graph{
        .allocator = gpa,
        .functions = .empty,
        .locations = .empty,
        .blocks = .empty,
        .insts = .empty,
        .scope = .root,
    };

    _ = try graph.flatten(tables, tree, tokens, 0, 0, 0);

    return graph;
}
