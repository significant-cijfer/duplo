const std = @import("std");

const parseInt = std.fmt.parseInt;

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
        tx_struct,
        function,

        fn supportsArith(self: Kind) bool {
            return switch (self) {
                .integer, .ct_integer => true,
                else => false,
            };
        }
    };

    const Extra = union {
        none: void,
        tx_type: Type,
        integer: Integer,
        tx_struct: Struct,
        function: Function,

        const Type = u32;

        const Integer = struct {
            sign: bool,
            bits: u16,
        };

        const Struct = struct {
            fields: u32,
            names: u32,
            len: u32,
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

//TODO, get this shi into every .init assignment
const Init = struct {
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        tx_type,
        integer,
    };

    const Extra = union {
        tx_type: u32,
        integer: i128,
    };
};

pub const Context = struct {
    allocator: Allocator,
    table: StringHashMap(Symbol),
    types: ArrayList(Typx),
    inits: ArrayList(Init),
    extra: ArrayList(u32),
    frame: Frame,

    const Symbol = struct {
        scope: Scope,
        typx: u32,
        init: u32,
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
            .inits = .empty,
            .extra = .empty,
            .frame = .{},
        };

        //NOTE, prevent idx 0, from being correct, thus preserving that spot as a NULL ref
        _ = try ctx.pushTypx(.VOID);
        _ = try ctx.pushInit(undefined);

        try ctx.put("i32", .{
            .scope = .global,
            .typx = try ctx.pushTypx(.{
                .kind = .tx_type,
                .extra = .{ .none = undefined },
            }),
            .init = try ctx.pushInit(.{
                .kind = .tx_type,
                .extra = .{ .tx_type = try ctx.pushTypx(.{
                    .kind = .integer,
                    .extra = .{ .integer = .{
                        .sign = true,
                        .bits = 32,
                    }},
                })},
            }),
        });

        try ctx.put("type", .{
            .scope = .global,
            .typx = try ctx.pushTypx(.{
                .kind = .tx_type,
                .extra = .{ .none = undefined },
            }),
            .init = try ctx.pushInit(.{
                .kind = .tx_type,
                .extra = .{ .tx_type = try ctx.pushTypx(.{
                    .kind = .tx_type,
                    .extra = .{ .none = undefined },
                })},
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
            .inits = try self.inits.clone(self.allocator),
            .extra = try self.extra.clone(self.allocator),
            .frame = self.frame,
        };
    }

    fn scope(self: *const Context) Scope {
        return if (self.frame.return_type == 0) .global else .local;
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

    fn pushInit(self: *Context, innit: Init) !u32 {
        const idx = self.inits.items.len;
        try self.inits.append(self.allocator, innit);
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
            .tx_struct => switch (ltyp.kind) {
                .tx_struct => {
                    const lfields = ltyp.extra.tx_struct.fields;
                    const llen = ltyp.extra.tx_struct.len;

                    const rfields = ltyp.extra.tx_struct.fields;
                    const rlen = ltyp.extra.tx_struct.len;

                    if (llen != rlen)
                        return false;

                    for (self.extra.items[lfields..lfields+llen], self.extra.items[rfields..rfields+rlen]) |lfield, rfield|
                        if (!self.castable(lfield, rfield))
                            return false;

                    return true;
                },
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

                const rtype = ctx.types.items[proto].extra.function.rtyp;
                ctx.frame.return_type = rtype;

                try ctx.put(slice, .{
                    .scope = .global,
                    .typx = proto,
                    .init = 0,
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

                const rinit = try self.eval(tree, tokens, source, node.extra.fproto.rtyp);
                const rtype = self.inits.items[rinit].extra.tx_type;

                return try self.pushTypx(.{
                    .kind = .function,
                    .extra = .{ .function = .{
                        .prms = try self.pushExtraList(prms.items),
                        .plen = @intCast(prms.items.len),
                        .rtyp = rtype,
                    } },
                });
            },
            .integer => {
                return try self.pushTypx(.INTEGER);
            },
            .identifier => {
                const slice = tokens.at(node.main).slice(source);
                const symbol = self.table.get(slice) orelse return error.UnrecognizedIdentifier;
                return symbol.typx;
            },
            .structdef => {
                var fields = ArrayList(u32).empty;
                defer fields.deinit(self.allocator);

                var names = ArrayList(u32).empty;
                defer names.deinit(self.allocator);

                const mmbrs = tree.extras(node.extra);

                for (mmbrs) |mmbr| {
                    try fields.append(
                        self.allocator,
                        try self.examine(tree, tokens, source, mmbr)
                    );

                    try names.append(
                        self.allocator,
                        tree.nodes.items[mmbr].main - 2,
                    );
                }

                return try self.pushTypx(.{
                    .kind = .tx_struct,
                    .extra = .{ .tx_struct = .{
                        .fields = try self.pushExtraList(fields.items),
                        .names = try self.pushExtraList(names.items),
                        .len = @intCast(fields.items.len),
                    }},
                });
            },
            .vardef => {
                const lhs = try self.eval(tree, tokens, source, node.extra.bin_op.lhs);
                const rhs = try self.eval(tree, tokens, source, node.extra.bin_op.rhs);

                const name = tokens.at(node.main+1).slice(source);
                const ltypx = self.inits.items[lhs].extra.tx_type;
                const rtypx = try self.examine(tree, tokens, source, node.extra.bin_op.rhs);

                if (!self.castable(ltypx, rtypx))
                    return error.FrameUncastableDef;

                try self.put(name, .{
                    .scope = self.scope(),
                    .typx = ltypx,
                    .init = rhs,
                });

                return ltypx;
            },
            .block => {
                const stmts = tree.extras(node.extra);

                for (stmts, 0..) |stmt, jdx| {
                    const sdx = try self.examine(tree, tokens, source, stmt);

                    switch (self.types.items[sdx].kind) {
                        .tx_noreturn => {
                            if (jdx < stmts.len-1) return error.FrameEarlyReturn;
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
            else => return error.UnhandledExamination,
        }
    }


    fn eval(self: *Context, tree: Ast, tokens: *Tokens, source: [:0]const u8, idx: u32) !u32 {
        const node = tree.nodes.items[idx];

        errdefer { if (error_idx == null) error_idx = idx; }

        switch (node.kind) {
            .integer => {
                const slice = tokens.at(node.main).slice(source);

                return try self.pushInit(.{
                    .kind = .integer,
                    .extra = .{ .integer = try parseInt(i128, slice, 0) },
                });
            },
            .identifier => {
                const slice = tokens.at(node.main).slice(source);
                const symbol = self.table.get(slice) orelse return error.UnrecognizedIdentifier;
                return if (symbol.init != 0) symbol.init else error.NonComptimeEval;
            },
            else => return error.UnhandledEval,
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
