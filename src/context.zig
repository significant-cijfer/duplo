const std = @import("std");
const panic = std.debug.panic;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMapUnmanaged;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

pub const Typx = struct {
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        integer,
        function,
    };

    const Extra = union {
        integer: Integer,
        function: Function,

        const Integer = struct {
            sign: bool,
            bits: u16,
        };

        const Function = struct {
        };
    };
};

pub const Context = struct {
    allocator: Allocator,
    table: StringHashMap(Symbol),
    types: ArrayList(Typx),

    const Symbol = struct {
        scope: Scope,
        typx: u32,
    };

    const Scope = enum {
        global,
        local,
        param,
    };

    pub fn deinit(self: *Context) void {
        self.table.deinit(self.allocator);
    }

    fn pushTypx(self: *Context, typx: Typx) !u32 {
        const idx = self.types.items.len;
        try self.types.append(self.allocator, typx);
        return @intCast(idx);
    }

    fn examine(self: *Context, tree: Ast, idx: u32) !u32 {
        const node = tree.nodes.items[idx];

        switch (node.kind) {
            .fdecl => {
                const proto = try self.examine(tree, node.extra.fdecl.proto);
                const body = try self.examine(tree, node.extra.fdecl.body);
                _ = body;

                return proto;
            },
            .fproto => {
                return try self.pushTypx(.{
                    .kind = .function,
                    .extra = .{ .function = .{} },
                });
            },
            else => panic("Unhandled examination: {}", .{node.kind}),
        }
    }
};

pub fn scan(gpa: Allocator, tree: Ast) !Context {
    var ctx = Context{
        .allocator = gpa,
        .table = .empty,
        .types = .empty,
    };

    const root = tree.nodes.items[0];
    const roots = tree.extras(root.extra);

    for (roots) |ndx| {
        const tdx = try ctx.examine(tree, ndx);
        _ = tdx;
    }

    return ctx;
}
