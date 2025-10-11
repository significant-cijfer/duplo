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
        tx_void,
        tx_noreturn,
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

    const VOID = Typx{
        .kind = .tx_void,
        .extra = undefined,
    };

    const NORETURN = Typx{
        .kind = .tx_noreturn,
        .extra = undefined,
    };
};

pub const Context = struct {
    allocator: Allocator,
    table: StringHashMap(Symbol),
    types: ArrayList(Typx),
    frame: Frame,

    const Symbol = struct {
        scope: Scope,
        typx: u32,
    };

    const Scope = enum {
        global,
        local,
        param,
    };

    const Frame = struct {
        return_type: u32 = 0,
        break_type: u32 = 0,
    };

    pub fn deinit(self: *Context) void {
        self.table.deinit(self.allocator);
    }

    fn clone(self: *Context) !Context {
        return .{
            .allocator = self.allocator,
            .table = try self.table.clone(self.allocator),
            .types = try self.types.clone(self.allocator),
        };
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
            .block => {
                const stmts = tree.extras(node.extra);

                for (stmts, 0..) |stmt, jdx| {
                    const sdx = try self.examine(tree, stmt);

                    switch (self.types.items[sdx].kind) {
                        .tx_noreturn => {
                            if (jdx < stmts.len-1) @panic("early return detected in block");
                            break;
                        },
                        else => {},
                    }
                }

                return try self.pushTypx(.VOID);
            },
            .ret => {
                //TODO(examine), check wether the return type is compatible
                const rtype = if (node.extra.mon_op == 0)
                    try self.pushTypx(.VOID)
                else
                    try self.examine(tree, node.extra.mon_op);

                _ = rtype;

                return try self.pushTypx(.NORETURN);
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
        .frame = .{},
    };

    //NOTE, prevent idx 0, from being correct, thus preserving that spot as a NULL ref
    _ = try ctx.pushTypx(undefined);

    const root = tree.nodes.items[0];
    const roots = tree.extras(root.extra);

    for (roots) |ndx| {
        const tdx = try ctx.examine(tree, ndx);
        _ = tdx;
    }

    return ctx;
}
