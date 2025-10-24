const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

pub var error_idx: ?u32 = null;

const Error = error {
    UnhandledBuild,
}
    || Allocator.Error;

pub const Graph = struct {
    allocator: Allocator,
    nodes: ArrayList(Node),
    extra: ArrayList(u32),

    pub fn deinit(self: *Graph) void {
        self.nodes.deinit(self.allocator);
        self.extra.deinit(self.allocator);
    }

    fn pushNode(self: *Graph, node: Node) !u32 {
        const idx = self.nodes.items.len;
        try self.nodes.append(self.allocator, node);
        return @intCast(idx);
    }

    fn pushExtra(self: *Graph, dep: u32) !u32 {
        const idx = self.extra.items.len;
        try self.extra.append(self.allocator, dep);
        return @intCast(idx);
    }

    fn pushExtraList(self: *Graph, deps: []u32) !u32 {
        const idx = self.extra.items.len;
        try self.extra.appendSlice(self.allocator, deps);
        return @intCast(idx);
    }

    fn build(self: *Graph, tree: Ast, idx: u32) !u32 {
        const node = tree.nodes.items[idx];

        errdefer { if (error_idx == null) error_idx = idx; }

        switch (node.kind) {
            .root => {
                const roots = tree.extras(node.extra);

                var deps = ArrayList(u32).empty;
                defer deps.deinit(self.allocator);

                for (roots) |root| {
                    const dep = try self.build(tree, root);
                    try deps.append(self.allocator, dep);
                }

                return try self.pushNode(.{
                    .main = idx,
                    .deps = try self.pushExtraList(deps.items),
                    .len = @intCast(deps.items.len),
                });
            },
            .fdecl => {
                //TODO(build, fdecl)
                const body = try self.build(tree, node.extra.fdecl.body);

                return try self.pushNode(.{
                    .main = idx,
                    .deps = try self.pushExtra(body),
                    .len = 1,
                });
            },
            .vardef => {
                //TODO(build, vardef)
                return 0;
            },
            .block => {
                //TODO(build, block)
                const stmts = tree.extras(node.extra);

                for (stmts) |stmt| {
                    _ = stmt;
                }

                return 0;
            },
            else => return error.UnhandledBuild,
        }
    }
};

const Node = struct {
    main: u32,
    deps: u32,
    len: u32,
};

pub fn construct(gpa: Allocator, tree: Ast) !Graph {
    var graph = Graph{
        .allocator = gpa,
        .nodes = .empty,
        .extra = .empty,
    };

    _ = try graph.build(tree, 0);

    return graph;
}
