const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lexer = @import("lexer.zig");
const Tokens = Lexer.Tokens;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

pub var error_idx: ?u32 = null;

const Error = error {
    UnhandledFlatten,
}
    || Allocator.Error;

pub const Graph = struct {
    allocator: Allocator,
    functions: ArrayList(Function),
    blocks: ArrayList(Block),
    insts: ArrayList(Inst),
    scope: Scope,

    const Scope = enum {
        root,
        function,
    };

    pub fn deinit(self: *Graph) void {
        self.blocks.deinit(self.allocator);
        self.insts.deinit(self.allocator);
    }

    fn reserveBlock(self: *Graph) !u32 {
        const idx = self.blocks.items.len;
        try self.blocks.append(self.allocator, .{
            .idx = self.insts.items.len,
            .len = undefined,
            .flow = undefined,
        });
        return idx;
    }

    fn flatten(self: *Graph, tree: Ast, tokens: Tokens, source: [:0]const u8, bdx: u32, idx: u32) !u32 {
        const node = tree.nodes.items[idx];
        var block = bdx;

        errdefer { if (error_idx == null) error_idx = idx; }

        switch (node.kind) {
            .root => {
                const roots = tree.extras(node.extra);

                for (roots) |root| {
                    //NOTE, block reassignment is technically useless here
                    //      but it feels nice to do it anyway
                    block = try self.flatten(tree, tokens, source, block, root);
                }

                return block;
            },
            .fdecl => {
                const name = tokens.at(node.main+1).slice(source);
                const root = try self.reserveBlock();

                try self.functions.append(self.allocator, .{
                    .name = name,
                    .root = root,
                });

                return try self.flatten(tree, tokens, source, root, node.extra.fdecl.body);
            },
            .integer => {
                //TODO(flatten), expand
                try self.insts.append(self.allocator, .{
                    .kind = .copy,
                });

                return block;
            },
            .identifier => {
                //TODO(flatten), expand
                try self.insts.append(self.allocator, .{
                    .kind = .copy,
                });

                return block;
            },
            .vardef => {
                if (self.scope == .root) return block;

                return error.UnhandledFlatten;
            },
            .block => {
                const stmts = tree.extras(node.extra);

                for (stmts) |stmt| {
                    block = try self.flatten(tree, tokens, source, block, stmt);
                }

                return block;
            },
            .add => {
                //TODO(flatten), expand
                block = try self.flatten(tree, tokens, source, block, node.extra.bin_op.lhs);
                block = try self.flatten(tree, tokens, source, block, node.extra.bin_op.rhs);
                try self.insts.append(self.allocator, .{
                    .kind = .add,
                });

                return block;
            },
            .ret => {
                block = try self.flatten(tree, tokens, source, block, node.extra.mon_op);

                self.blocks.items[bdx] = .{
                    .idx = self.blocks.items[bdx].idx,
                    .len = self.insts.items.len - self.blocks.items[bdx].idx,
                    .flow = .{
                        .kind = .ret,
                        .extra = .{ .none = undefined },
                    },
                };

                return block;
            },
            else => return error.UnhandledFlatten,
        }
    }

    pub fn debug(self: Graph) void {
        for (self.functions.items) |function| {
            std.log.info("Function: {s}", .{function.name});

            const block = self.blocks.items[function.root];
            for (self.insts.items[block.idx..block.idx+block.len]) |inst| {
                std.log.info("  {any}", .{inst.kind});
            }
            std.log.info("  {any}", .{block.flow.kind});
        }
    }
};

const Function = struct {
    name: []const u8,
    root: u32,
};

const Block = struct {
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
        none: void,
        mono: u32,
        cond: Cond,

        const Cond = struct {
            lhs: u32,
            rhs: u32,
        };
    };
};

const Inst = struct {
    kind: Kind,

    const Kind = enum {
        copy,
        add,
    };
};

pub fn construct(gpa: Allocator, tree: Ast, tokens: Tokens, source: [:0]const u8) !Graph {
    var graph = Graph{
        .allocator = gpa,
        .functions = .empty,
        .blocks = .empty,
        .insts = .empty,
        .scope = .root,
    };

    _ = try graph.flatten(tree, tokens, source, 0, 0);

    return graph;
}
