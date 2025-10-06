const std = @import("std");
const panic = std.debug.panic;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

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
    table: StringHashMap(Symbol),
    types: ArrayList(Typx),

    const Symbol = struct {
        scope: Scope,
        typx: Typx,
    };

    const Scope = enum {
        global,
        local,
        param,
    };

    pub fn deinit(self: *Context) void {
        self.table.deinit();
    }

    fn pushTypx(self: *Context, typx: Typx) !u32 {
    }

    fn examine(self: *Context, tree: Ast, idx: u32) !u32 {
        const node = tree.nodes.items[idx];

        _ = self;

        switch (node.kind) {
            .fdecl => {
                return 0;
            },
            else => panic("Unhandled examination: {}", .{node.kind}),
        }
    }
};

pub fn scan(gpa: Allocator, tree: Ast) !Context {
    var ctx = Context{
        .table = .init(gpa),
        .types = .init(gpa),
    };

    const root = tree.nodes.items[0];
    const roots = tree.extras(root.extra);

    for (roots) |ndx| {
        const tdx = try ctx.examine(tree, ndx);
        _ = tdx;
    }

    return ctx;
}
