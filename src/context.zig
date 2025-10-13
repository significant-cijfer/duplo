const std = @import("std");
const panic = std.debug.panic;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMapUnmanaged;

const Lexer = @import("lexer.zig");
const Tokens = Lexer.Tokens;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;
const Node = Parser.Node;

pub var error_idx: ?u32 = null;

pub const Typx = struct {
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        tx_type,
        tx_void,
        tx_noreturn,
        integer,
        ct_integer,
        function,

        fn supportsArith(self: Kind) bool {
            return switch (self) {
                .integer, .ct_integer => true,
                else => false,
            };
        }
    };

    const Extra = union {
        tx_type: Type,
        integer: Integer,
        function: Function,

        const Type = u32;

        const Integer = struct {
            sign: bool,
            bits: u16,
        };

        const Function = struct {
            prms: u32,
            plen: u32,
            rtyp: u32,
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

    const INTEGER = Typx{
        .kind = .ct_integer,
        .extra = undefined,
    };
};

pub const Context = struct {
    allocator: Allocator,
    table: StringHashMap(Symbol),
    types: ArrayList(Typx),
    extra: ArrayList(u32),
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

    pub fn init(gpa: Allocator) !Context {
        var ctx = Context{
            .allocator = gpa,
            .table = .empty,
            .types = .empty,
            .extra = .empty,
            .frame = .{},
        };

        //NOTE, prevent idx 0, from being correct, thus preserving that spot as a NULL ref
        _ = try ctx.pushTypx(undefined);

        try ctx.put("i32", .{
            .scope = .global,
            .typx = try ctx.pushTypx(.{
                .kind = .tx_type,
                .extra = .{ .tx_type = try ctx.pushTypx(.{
                    .kind = .integer,
                    .extra = .{ .integer = .{
                        .sign = true,
                        .bits = 32,
                    }},
                }) },
            }),
        });

        return ctx;
    }

    pub fn deinit(self: *Context) void {
        self.table.deinit(self.allocator);
    }

    fn clone(self: *Context) !Context {
        return .{
            .allocator = self.allocator,
            .table = try self.table.clone(self.allocator),
            .types = try self.types.clone(self.allocator),
            .extra = try self.extra.clone(self.allocator),
            .frame = self.frame,
        };
    }

    fn put(self: *Context, key: []const u8, value: Symbol) !void {
        return self.table.put(self.allocator, key, value);
    }

    fn get(self: *Context, key: []const u8) ?Symbol {
        return self.table.get(key);
    }

    fn pushTypx(self: *Context, typx: Typx) !u32 {
        const idx = self.types.items.len;
        try self.types.append(self.allocator, typx);
        return @intCast(idx);
    }

    fn pushExtraList(self: *Context, typxs: []u32) !u32 {
        const idx = self.extra.items.len;
        try self.extra.appendSlice(self.allocator, typxs);
        return @intCast(idx);
    }


    //NOTE, rhs has to "transform" into lhs
    fn castable(self: *const Context, lhs: u32, rhs: u32) bool {
        const ltyp = self.types.items[lhs];
        const rtyp = self.types.items[rhs];

        return switch (rtyp.kind) {
            .tx_type => switch (ltyp.kind) {
                .tx_type => true,
                else => false,
            },
            .tx_void => switch (ltyp.kind) {
                .tx_void => true,
                else => false,
            },
            .tx_noreturn => true,
            .integer => switch (ltyp.kind) {
                .integer => {
                    if (ltyp.extra.integer.sign != rtyp.extra.integer.sign)
                        return false;

                    return ltyp.extra.integer.bits >= rtyp.extra.integer.bits;
                },
                else => false,
            },
            .ct_integer => switch (ltyp.kind) {
                .integer => true,
                .ct_integer => true,
                else => false,
            },
            .function => switch (ltyp.kind) {
                .function => {
                    const lprms = ltyp.extra.function.prms;
                    const lplen = ltyp.extra.function.plen;

                    const rprms = rtyp.extra.function.prms;
                    const rplen = rtyp.extra.function.plen;

                    if (lplen != rplen)
                        return false;

                    for (self.extra.items[lprms..lprms+lplen], self.extra.items[rprms..rprms+rplen]) |lprm, rprm|
                        if (!self.castable(lprm, rprm))
                            return false;

                    return self.castable(ltyp.extra.function.rtyp, rtyp.extra.function.rtyp);
                },
                else => false,
            },
        };
    }

    fn examine(self: *Context, tree: Ast, tokens: *Tokens, source: [:0]const u8, idx: u32) !u32 {
        const node = tree.nodes.items[idx];

        errdefer { if (error_idx == null) error_idx = idx; }

        switch (node.kind) {
            .fdecl => {
                const proto = try self.examine(tree, tokens, source, node.extra.fdecl.proto);
                const slice = tokens.at(node.main+1).slice(source);

                var ctx = try self.clone();
                defer ctx.deinit();

                const prot = ctx.types.items[proto].extra.function.rtyp;
                const rtyp = ctx.types.items[prot].extra.tx_type;

                ctx.frame.return_type = rtyp;

                try ctx.put(slice, .{
                    .scope = .global,
                    .typx = proto,
                });

                const body = try ctx.examine(tree, tokens, source, node.extra.fdecl.body);
                _ = body;

                return proto;
            },
            .fproto => {
                var prms = ArrayList(u32).empty;
                defer prms.deinit(self.allocator);

                const qrms = tree.nodes.items[node.extra.fproto.prms];
                for (tree.extras(qrms.extra)) |qrm| {
                    try prms.append(
                        self.allocator,
                        try self.examine(tree, tokens, source, qrm),
                    );
                }

                const rtyp = try self.examine(tree, tokens, source, node.extra.fproto.rtyp);

                return try self.pushTypx(.{
                    .kind = .function,
                    .extra = .{ .function = .{
                        .prms = try self.pushExtraList(prms.items),
                        .plen = @intCast(prms.items.len),
                        .rtyp = rtyp,
                    } },
                });
            },
            .integer => {
                return try self.pushTypx(.INTEGER);
            },
            .identifier => {
                const slice = tokens.at(node.main).slice(source);
                const symbol = self.table.get(slice) orelse panic("Unrecognized identifier: \x1B[31m{s}\x1B[0m", .{slice});
                return symbol.typx;
            },
            .block => {
                const stmts = tree.extras(node.extra);

                for (stmts, 0..) |stmt, jdx| {
                    const sdx = try self.examine(tree, tokens, source, stmt);

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
            .add, .sub, .mul, .div => {
                const lhs = try self.examine(tree, tokens, source, node.extra.bin_op.lhs);
                const rhs = try self.examine(tree, tokens, source, node.extra.bin_op.rhs);

                const ltyp = self.types.items[lhs];
                const rtyp = self.types.items[rhs];

                if (!ltyp.kind.supportsArith() or !rtyp.kind.supportsArith())
                    return error.ArithNonInteger;

                if (!self.castable(lhs, rhs))
                    return error.ArithNonCastable;

                return lhs;
            },
            .ret => {
                const rtype = if (node.extra.mon_op == 0)
                    try self.pushTypx(.VOID)
                else
                    try self.examine(tree, tokens, source, node.extra.mon_op);

                if (self.frame.return_type == 0)
                    return error.FrameNonReturn;

                if (!self.castable(self.frame.return_type, rtype))
                    return error.FrameUncastableReturn;

                return try self.pushTypx(.NORETURN);
            },
            else => panic("Unhandled examination: {}", .{node.kind}),
        }
    }
};

pub fn scan(gpa: Allocator, tree: Ast, tokens: *Tokens, source: [:0]const u8) !Context {
    var ctx = try Context.init(gpa);

    const root = tree.nodes.items[0];
    const roots = tree.extras(root.extra);

    for (roots) |ndx| {
        const tdx = try ctx.examine(tree, tokens, source, ndx);
        _ = tdx;
    }

    return ctx;
}
