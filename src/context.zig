const std = @import("std");

const eql = std.mem.eql;
const parseInt = std.fmt.parseInt;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayHashMap = std.AutoArrayHashMap;
const StringHashMap = std.StringHashMapUnmanaged;

const Lexer = @import("lexer.zig");
const Tokens = Lexer.Tokens;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;
const Node = Parser.Node;

pub var error_idx: ?u32 = null;

const Error = error {
    DestroyedLinearInstanceUse,
    IncompatibleStructLitShape,
    NonInitializedStructField,
    NonStructStructLitHead,
    NonCastableStructLit,
    NonFunctionFunctionCall,
    NonCastableFunctionArg,
    NonCastableTernaryCond,
    NonCastableTernaryBranch,
    UnrecognizedIdentifier,
    UnhandledExamination,
    FrameNonDestroyedLinearInstance,
    FrameUndestroyableInstance,
    FrameRootLinearDef,
    FrameUncastableReturn,
    FrameUncastableDef,
    FrameEarlyReturn,
    FrameNonReturn,
    FrameNoreturn,
    FrameNonDestroy,
    ArithNonCastable,
    ArithNonInteger,
    VariableShadowed,
    NonComptimeEval,
    UnhandledEval,
}
    || std.fmt.ParseIntError
    || Allocator.Error;

pub const Typx = struct {
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        tx_type,
        tx_void,
        tx_noreturn,
        tx_undefined,
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
            attr: u32, //NOTE, unique to each structdef instance
            fields: u32,
            names: u32,
            len: u32,
        };

        const Function = struct {
            names: u32,
            prms: u32,
            plen: u32,
            rtyp: u32,
        };
    };

    pub const VOID = Typx{
        .kind = .tx_void,
        .extra = .{ .none = undefined },
    };

    pub const TYPE = Typx{
        .kind = .tx_type,
        .extra = .{ .none = undefined },
    };

    pub const NORETURN = Typx{
        .kind = .tx_noreturn,
        .extra = .{ .none = undefined },
    };

    pub const UNDEFINED = Typx{
        .kind = .tx_undefined,
        .extra = .{ .none = undefined },
    };

    pub const INTEGER = Typx{
        .kind = .ct_integer,
        .extra = .{ .none = undefined },
    };
};

const Init = struct {
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        tx_type,
        integer,
        structlit,
    };

    const Extra = union {
        tx_type: u32,
        integer: i128,
        structlit: StructLit,

        const StructLit = struct {
            names: u32,
            inits: u32,
            len: u32,
        };
    };
};

const Attr = union {
    tx_struct: Struct,

    const Struct = struct {
        linear: bool,
    };
};

pub const Tables = struct {
    table: ArrayHashMap(u32, Table),

    pub fn init(gpa: Allocator) Tables {
        return .{
            .table = .init(gpa),
        };
    }

    pub fn deinit(self: *Tables) void {
        for (self.table.values()) |*table|
            table.deinit();

        self.table.deinit();
    }

    fn put(self: *Tables, node: u32, table: Table) !void {
        try self.table.put(node, table);
    }

    pub fn get(self: Tables, node: u32) ?Table {
        return self.table.get(node);
    }
};

pub const Table = struct {
    allocator: Allocator,
    table: StringHashMap(Symbol),
    types: ArrayList(Typx),
    inits: ArrayList(Init),
    attrs: ArrayList(Attr),
    extra: ArrayList(u32),
    frame: Frame,

    const Symbol = struct {
        storage: Storage,
        status: Status = .alive,
        typx: u32,
        init: u32,
    };

    const Status = enum {
        alive,
        used,
    };

    const Storage = enum {
        none,
        auto,
        public,
        //indirect,
    };

    const Frame = struct {
        return_type: u32 = 0,
        break_type: u32 = 0,
    };

    pub fn init(gpa: Allocator) !Table {
        var table = Table{
            .allocator = gpa,
            .table = .empty,
            .types = .empty,
            .inits = .empty,
            .attrs = .empty,
            .extra = .empty,
            .frame = .{},
        };

        //NOTE, prevent idx 0, from being correct, thus preserving that spot as a NULL ref
        _ = try table.pushTypx(.VOID);
        _ = try table.pushInit(undefined);

        try table.put("u8", .{
            .storage = .none,
            .typx = try table.pushTypx(.{
                .kind = .tx_type,
                .extra = .{ .none = undefined },
            }),
            .init = try table.pushInit(.{
                .kind = .tx_type,
                .extra = .{ .tx_type = try table.pushTypx(.{
                    .kind = .integer,
                    .extra = .{ .integer = .{
                        .sign = false,
                        .bits = 8,
                    }},
                })},
            }),
        });

        try table.put("i32", .{
            .storage = .none,
            .typx = try table.pushTypx(.{
                .kind = .tx_type,
                .extra = .{ .none = undefined },
            }),
            .init = try table.pushInit(.{
                .kind = .tx_type,
                .extra = .{ .tx_type = try table.pushTypx(.{
                    .kind = .integer,
                    .extra = .{ .integer = .{
                        .sign = true,
                        .bits = 32,
                    }},
                })},
            }),
        });

        try table.put("type", .{
            .storage = .none,
            .typx = try table.pushTypx(.{
                .kind = .tx_type,
                .extra = .{ .none = undefined },
            }),
            .init = try table.pushInit(.{
                .kind = .tx_type,
                .extra = .{ .tx_type = try table.pushTypx(.{
                    .kind = .tx_type,
                    .extra = .{ .none = undefined },
                })},
            }),
        });

        return table;
    }

    pub fn deinit(self: *Table) void {
        self.table.deinit(self.allocator);
    }

    fn clone(self: *Table) !Table {
        return .{
            .allocator = self.allocator,
            .table = try self.table.clone(self.allocator),
            .types = try self.types.clone(self.allocator),
            .inits = try self.inits.clone(self.allocator),
            .attrs = try self.attrs.clone(self.allocator),
            .extra = try self.extra.clone(self.allocator),
            .frame = self.frame,
        };
    }

    fn storage(self: *const Table) Storage {
        return if (self.frame.return_type == 0) .public else .auto;
    }

    fn put(self: *Table, key: []const u8, value: Symbol) !void {
        if (self.table.contains(key))
            return error.VariableShadowed;

        return self.table.putNoClobber(self.allocator, key, value);
    }

    pub fn get(self: Table, key: []const u8) ?Symbol {
        return self.table.get(key);
    }

    fn pushTypx(self: *Table, typx: Typx) !u32 {
        const idx = self.types.items.len;
        try self.types.append(self.allocator, typx);
        return @intCast(idx);
    }

    fn pushExtraList(self: *Table, typxs: []u32) !u32 {
        const idx = self.extra.items.len;
        try self.extra.appendSlice(self.allocator, typxs);
        return @intCast(idx);
    }

    fn pushInit(self: *Table, innit: Init) !u32 {
        const idx = self.inits.items.len;
        try self.inits.append(self.allocator, innit);
        return @intCast(idx);
    }

    fn pushAttr(self: *Table, attr: Attr) !u32 {
        const idx = self.attrs.items.len;
        try self.attrs.append(self.allocator, attr);
        return @intCast(idx);
    }

    pub fn extras(self: *const Table, idx: u32, len: u32) []u32 {
        return self.extra.items[idx..idx+len];
    }

    fn isLinear(self: *const Table, typx: u32) bool {
        const ltyp = self.types.items[typx];
        return switch (ltyp.kind) {
            .tx_struct => {
                const attr = ltyp.extra.tx_struct.attr;
                return self.attrs.items[attr].tx_struct.linear;
            },
            else => false,
        };
    }

    fn isInteger(self: *const Table, typx: u32) bool {
        const item = self.types.items[typx];
        return switch (item.kind) {
            .ct_integer => true,
            .integer => true,
            else => false,
        };
    }

    //NOTE, rhs has to "transform" into lhs
    fn castable(self: *const Table, lhs: u32, rhs: u32) bool {
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
            .tx_undefined => switch (ltyp.kind) {
                .tx_undefined => true,
                else => false,
            },
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
                .tx_struct => ltyp.extra.tx_struct.attr == rtyp.extra.tx_struct.attr,
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

                    for (self.extras(lprms, lplen), self.extras(rprms, rplen)) |lprm, rprm|
                        if (!self.castable(lprm, rprm))
                            return false;

                    return self.castable(ltyp.extra.function.rtyp, rtyp.extra.function.rtyp);
                },
                else => false,
            },
        };
    }

    fn examine(self: *Table, tables: *Tables, tree: Ast, tokens: Tokens, idx: u32) Error!u32 {
        const node = tree.nodes.items[idx];

        errdefer { if (error_idx == null) error_idx = idx; }

        switch (node.kind) {
            .root => {
                const roots = tree.extras(node.extra);

                for (roots) |root| {
                    const sdx = try self.examine(tables, tree, tokens, root);
                    _ = sdx;
                }

                return try self.pushTypx(.VOID);
            },
            .fdecl => {
                const proto = try self.examine(tables, tree, tokens, node.extra.fdecl.proto);
                const slice = tokens.slice(node.main+1);

                var table = try self.clone();
                //defer table.deinit();

                const typx = table.types.items[proto].extra.function;
                const names = table.extra.items[typx.names..typx.names+typx.plen];
                const prms = table.extra.items[typx.prms..typx.prms+typx.plen];

                for (names, prms) |name, prm| {
                    const pslice = tokens.slice(name);

                    try table.put(pslice, .{
                        .storage = .auto,
                        .typx = prm,
                        .init = 0,
                    });
                }

                const rtype = table.types.items[proto].extra.function.rtyp;
                table.frame.return_type = rtype;

                try self.put(slice, .{
                    .storage = .public,
                    .typx = proto,
                    .init = 0,
                });

                try table.put(slice, .{
                    .storage = .public,
                    .typx = proto,
                    .init = 0,
                });

                const body = try table.examine(tables, tree, tokens, node.extra.fdecl.body);
                if (table.types.items[body].kind != .tx_noreturn)
                    return error.FrameNoreturn;

                try tables.put(idx, table);
                return proto;
            },
            .fproto => {
                var names = ArrayList(u32).empty;
                defer names.deinit(self.allocator);

                var prms = ArrayList(u32).empty;
                defer prms.deinit(self.allocator);

                const qrms = tree.nodes.items[node.extra.fproto.prms];
                for (tree.extras(qrms.extra)) |qrm| {
                    try names.append(
                        self.allocator,
                        tree.nodes.items[qrm].main - 2,
                    );

                    const qinit = try self.eval(tree, tokens, qrm);
                    const qtypx = self.inits.items[qinit].extra.tx_type;

                    try prms.append(
                        self.allocator,
                        qtypx,
                    );
                }

                const rinit = try self.eval(tree, tokens, node.extra.fproto.rtyp);
                const rtype = self.inits.items[rinit].extra.tx_type;

                return try self.pushTypx(.{
                    .kind = .function,
                    .extra = .{ .function = .{
                        .names = try self.pushExtraList(names.items),
                        .prms = try self.pushExtraList(prms.items),
                        .plen = @intCast(prms.items.len),
                        .rtyp = rtype,
                    } },
                });
            },
            .fcall => {
                const typx = try self.examine(tables, tree, tokens, node.extra.fcall.func);
                const proto = self.types.items[typx];

                if (proto.kind != .function)
                    return error.NonFunctionFunctionCall;

                const list = tree.nodes.items[node.extra.fcall.args];
                const args = tree.extras(list.extra);
                const prms = proto.extra.function.prms;
                const plen = proto.extra.function.plen;

                for (self.extras(prms, plen), args) |prm, arg| {
                    const atyp = try self.examine(tables, tree, tokens, arg);

                    if (!self.castable(prm, atyp))
                        return error.NonCastableFunctionArg;
                }

                return proto.extra.function.rtyp;
            },
            .integer => {
                return try self.pushTypx(.INTEGER);
            },
            .identifier => {
                const slice = tokens.slice(node.main);
                const symbol = self.table.get(slice) orelse return error.UnrecognizedIdentifier;

                if (symbol.status == .used)
                    return error.DestroyedLinearInstanceUse;

                if (self.isLinear(symbol.typx))
                    self.table.getPtr(slice).?.status = .used;

                return symbol.typx;
            },
            .structdef => {
                return try self.pushTypx(.TYPE);
            },
            .structlit => {
                var inits = StringHashMap(u32).empty;
                defer inits.deinit(self.allocator);

                const defs = tree.nodes.items[node.extra.structlit.defs];
                const mmbrs = tree.extras(defs.extra);

                for (mmbrs) |mmbr| {
                    const main = tree.nodes.items[mmbr].main - 2;
                    const name = tokens.slice(main);

                    try inits.put(
                        self.allocator,
                        name,
                        try self.examine(tables, tree, tokens, mmbr)
                    );
                }

                const head = try self.eval(tree, tokens, node.extra.structlit.head);
                const hmit = self.inits.items[head];
                const htype = switch (hmit.kind) {
                    .tx_type => hmit.extra.tx_type,
                    else => return error.NonStructStructLitHead,
                };

                const struc = self.types.items[htype].extra.tx_struct;
                const fields = self.extras(struc.fields, struc.len);
                const names = self.extras(struc.names, struc.len);
                if (struc.len != inits.count())
                    return error.IncompatibleStructLitShape;

                for (fields, names) |field, main| {
                    const name = tokens.slice(main);
                    const typx = inits.get(name) orelse return error.NonInitializedStructField;

                    if (!self.castable(self.inits.items[field].extra.tx_type, typx))
                        return error.NonCastableStructLit;
                }

                return htype;
            },
            .vardef => {
                const lhs = try self.eval(tree, tokens, node.extra.bin_op.lhs);
                const rhs = try self.eval(tree, tokens, node.extra.bin_op.rhs);

                const name = tokens.slice(node.main+1);
                const ltypx = self.inits.items[lhs].extra.tx_type;
                const rtypx = try self.examine(tables, tree, tokens, node.extra.bin_op.rhs);

                if (!self.castable(ltypx, rtypx))
                    return error.FrameUncastableDef;

                if (self.frame.return_type == 0 and self.isLinear(ltypx))
                    return error.FrameRootLinearDef;

                try self.put(name, .{
                    .storage = self.storage(),
                    .typx = ltypx,
                    .init = rhs,
                });

                return ltypx;
            },
            .block => {
                var table = try self.clone();
                //defer table.deinit();

                var typx = Typx.VOID;
                const stmts = tree.extras(node.extra);

                for (stmts, 0..) |stmt, jdx| {
                    const sdx = try table.examine(tables, tree, tokens, stmt);

                    switch (table.types.items[sdx].kind) {
                        .tx_noreturn => {
                            if (jdx < stmts.len-1) return error.FrameEarlyReturn;
                            typx = .NORETURN;
                            break;
                        },
                        else => {},
                    }
                }

                var symbols = table.table.iterator();
                while (symbols.next()) |symbol| {
                    if (!self.table.contains(symbol.key_ptr.*) and table.isLinear(symbol.value_ptr.typx) and symbol.value_ptr.status == .alive)
                        return error.FrameNonDestroyedLinearInstance;
                }

                try tables.put(idx, table);
                return try self.pushTypx(typx);
            },
            .add, .sub, .mul, .div => {
                const lhs = try self.examine(tables, tree, tokens, node.extra.bin_op.lhs);
                const rhs = try self.examine(tables, tree, tokens, node.extra.bin_op.rhs);

                const ltyp = self.types.items[lhs];
                const rtyp = self.types.items[rhs];

                if (!ltyp.kind.supportsArith() or !rtyp.kind.supportsArith())
                    return error.ArithNonInteger;

                if (!self.castable(lhs, rhs))
                    return error.ArithNonCastable;

                return lhs;
            },
            .ternary => {
                const chs = try self.examine(tables, tree, tokens, node.extra.tri_op.lhs);
                const lhs = try self.examine(tables, tree, tokens, node.extra.tri_op.mhs+0);
                const rhs = try self.examine(tables, tree, tokens, node.extra.tri_op.mhs+1);

                if (!self.isInteger(chs))
                    return error.NonCastableTernaryCond;

                if (!self.castable(lhs, rhs))
                    return error.NonCastableTernaryBranch;

                return lhs;
            },
            .destroy => {
                const rtype = try self.examine(tables, tree, tokens, node.extra.mon_op);

                if (self.frame.return_type == 0)
                    return error.FrameNonDestroy;

                if (!self.isLinear(rtype))
                    return error.FrameUndestroyableInstance;

                return try self.pushTypx(.VOID);
            },
            .ret => {
                const rtype = if (node.extra.mon_op == 0)
                    try self.pushTypx(.VOID)
                else
                    try self.examine(tables, tree, tokens, node.extra.mon_op);

                if (self.frame.return_type == 0)
                    return error.FrameNonReturn;

                if (!self.castable(self.frame.return_type, rtype))
                    return error.FrameUncastableReturn;

                return try self.pushTypx(.NORETURN);
            },
            else => return error.UnhandledExamination,
        }
    }


    fn eval(self: *Table, tree: Ast, tokens: Tokens, idx: u32) Error!u32 {
        const node = tree.nodes.items[idx];

        errdefer { if (error_idx == null) error_idx = idx; }

        switch (node.kind) {
            .integer => {
                const slice = tokens.slice(node.main);

                return try self.pushInit(.{
                    .kind = .integer,
                    .extra = .{ .integer = try parseInt(i128, slice, 0) },
                });
            },
            .identifier => {
                //NOTE, linear types should be impossible here
                //      since they aren't definable in the root frame
                const slice = tokens.slice(node.main);
                const symbol = self.table.get(slice) orelse return error.UnrecognizedIdentifier;
                return if (symbol.init != 0) symbol.init else error.NonComptimeEval;
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
                        try self.eval(tree, tokens, mmbr)
                    );

                    try names.append(
                        self.allocator,
                        tree.nodes.items[mmbr].main - 2,
                    );
                }

                const attr = Attr{ .tx_struct = .{
                    .linear = tokens.at(node.main+1).kind == .@"linear",
                }};

                const typx = try self.pushTypx(.{
                    .kind = .tx_struct,
                    .extra = .{ .tx_struct = .{
                        .attr = try self.pushAttr(attr),
                        .fields = try self.pushExtraList(fields.items),
                        .names = try self.pushExtraList(names.items),
                        .len = @intCast(fields.items.len),
                    }},
                });

                return try self.pushInit(.{
                    .kind = .tx_type,
                    .extra = .{ .tx_type = typx },
                });
            },
            .structlit => {
                var names = ArrayList(u32).empty;
                defer names.deinit(self.allocator);

                var inits = ArrayList(u32).empty;
                defer inits.deinit(self.allocator);

                const defs = tree.nodes.items[node.extra.structlit.defs];
                const mmbrs = tree.extras(defs.extra);

                for (mmbrs) |mmbr| {
                    try names.append(
                        self.allocator,
                        tree.nodes.items[mmbr].main - 2,
                    );

                    try inits.append(
                        self.allocator,
                        try self.eval(tree, tokens, mmbr)
                    );
                }

                for (inits.items) |ev|
                    if (ev == 0) return 0;

                return try self.pushInit(.{
                    .kind = .structlit,
                    .extra = .{ .structlit = .{
                        .names = try self.pushExtraList(names.items),
                        .inits = try self.pushExtraList(inits.items),
                        .len = @intCast(inits.items.len),
                    }},
                });
            },
            .add,
            .sub,
            .mul,
            .div => return 0,
            .ternary => return 0,
            else => return error.UnhandledEval,
        }
    }
};

pub fn scan(gpa: Allocator, tree: Ast, tokens: Tokens) !Tables {
    var tables = Tables.init(gpa);
    var table = try Table.init(gpa);

    _ = try table.examine(&tables, tree, tokens, 0);

    try tables.put(0, table);
    return tables;
}
