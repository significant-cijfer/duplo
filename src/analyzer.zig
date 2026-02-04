const std = @import("std");

const parseInt = std.fmt.parseInt;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayHashMap = std.AutoArrayHashMapUnmanaged;
const StringHashMap = std.StringHashMapUnmanaged;

const Lexer = @import("lexer.zig");
const Tokens = Lexer.Tokens;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

const Int = u32;
const Vdx = u32;
const BigInt = std.math.big.int.Managed;

const Error = error {
    UndefinedTable,
    UndefinedKey,
    ShadowedKey,
    OutOfBounds,
    RuntimeEval,
    EarlyReturn,
    UnspecifiedReturn,
    UncastableReturn,
    UnspecifiedBreak,
    UncastableBreak,
    IncompatibleTypes,
    NonIntegerTerm,
    UncallableTerm,
}
    || error { TODO_Eval }
    || Allocator.Error
    || std.fmt.ParseIntError
    || error { InvalidBase, InvalidCharacter };

pub var error_idx: ?u32 = null;

pub const Context = struct {
    allocator: Allocator,
    tables: ArrayHashMap(Int, Table),
    constants: ArrayList(Constant),
    typxs: ArrayList(Typx),
    extra: ArrayList(Int),

    fn init(gpa: Allocator) !Context {
        var context = Context{
            .allocator = gpa,
            .tables = .empty,
            .constants = .empty,
            .typxs = .empty,
            .extra = .empty,
        };

        try context.tables.put(gpa, 0, .{
            .parent = null,
            .symbols = .empty,
            .storage = .root,
            .frame = .root,
        });

        //NOTE, prevent idx 0, from being correct, thus preserving that spot as a NULL ref
        _ = try context.add(Constant{ .typx = 0 });
        _ = try context.add(@as(Typx, undefined));

        try context.put(0, "u8", .{
            .typx = try context.add(Typx{ .typx = {} }),
            .con = try context.add(Constant{ .typx = try context.add(Typx{ .int = .{
                .bits = 8,
                .sign = false,
            } }) }),
        });

        try context.put(0, "i32", .{
            .typx = try context.add(Typx{ .typx = {} }),
            .con = try context.add(Constant{ .typx = try context.add(Typx{ .int = .{
                .bits = 32,
                .sign = true,
            } }) }),
        });

        try context.put(0, "type", .{
            .typx = try context.add(Typx{ .typx = {} }),
            .con = try context.add(Constant{ .typx = try context.add(Typx{ .typx = {} }) }),
        });

        return context;
    }

    pub fn deinit(self: *Context) void {
        for (self.tables.values()) |*table|
            table.deinit(self.allocator);

        for (self.constants.items) |*con|
            con.deinit();

        self.tables.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.typxs.deinit(self.allocator);
        self.extra.deinit(self.allocator);
    }

    fn birth(self: *Context, table: Int, node: Int, storage: Storage, ret: ?Int, brk: ?Int) !Int {
        const t = self.tables.get(table)
            orelse return error.UndefinedTable;

        const child = Table{
            .parent = table,
            .symbols = .empty,
            .storage = storage,
            .frame = .{
                .ret = ret orelse t.frame.ret,
                .brk = brk orelse t.frame.brk,
            },
        };

        try self.tables.put(self.allocator, node, child);
        return node;
    }

    fn listOf(self: Context, comptime T: type) []const T {
        return switch (T) {
            Constant => self.constants.items,
            Typx => self.typxs.items,
            Int => self.extra.items,
            else => @compileError("No list exists of type: " + @tagName(T)),
        };
    }

    fn arrayOf(self: *Context, comptime T: type) *ArrayList(T) {
        return switch (T) {
            Constant => &self.constants,
            Typx => &self.typxs,
            Int => &self.extra,
            else => @compileError("No list exists of type: " + @tagName(T)),
        };
    }

    pub fn at(self: Context, comptime T: type, idx: Int) T {
        return self.listOf(T)[idx];
    }

    pub fn slice(self: Context, comptime T: type, idx: Int, len: Int) []const T {
        return self.listOf(T)[idx..idx+len];
    }

    fn add(self: *Context, value: anytype) !Int {
        const arr = self.arrayOf(@TypeOf(value));
        const idx = arr.items.len;

        try arr.append(self.allocator, value);
        return @intCast(idx);
    }

    fn addSlice(self: *Context, value: anytype) !Int {
        const arr = self.arrayOf(std.meta.Child(@TypeOf(value)));
        const idx = arr.items.len;

        try arr.appendSlice(self.allocator, value);
        return @intCast(idx);
    }

    pub fn get(self: Context, table: Int, key: []const u8) !Symbol {
        const t = self.tables.get(table)
            orelse return error.UndefinedTable;

        return t.symbols.get(key)
            orelse if (t.parent) |parent| self.get(parent, key)
                else error.UndefinedKey;
    }

    fn put(self: *Context, table: Int, key: []const u8, symbol: Symbol) !void {
        const t = self.tables.getPtr(table)
            orelse return error.UndefinedTable;

        if (t.symbols.contains(key))
            return error.ShadowedKey;

        return t.symbols.putNoClobber(self.allocator, key, symbol);
    }

    fn frameReturns(self: Context, table: Int) !bool {
        const t = self.tables.get(table)
            orelse return error.UndefinedTable;

        return t.frame.ret != 0;
    }

    fn frameBreaks(self: Context, table: Int) !bool {
        const t = self.tables.get(table)
            orelse return error.UndefinedTable;

        return t.frame.brk != 0;
    }

    fn returnable(self: Context, table: Int, typx: Int) !bool {
        const t = self.tables.get(table)
            orelse return error.UndefinedTable;

        if (!try self.frameReturns(table))
            return error.UnspecifiedReturn;

        return self.castable(t.frame.ret, typx);
    }

    fn breakable(self: Context, table: Int, typx: Int) !bool {
        const t = self.tables.get(table)
            orelse return error.UndefinedTable;

        if (!try self.frameBreaks(table))
            return error.UnspecifiedBreak;

        return self.castable(t.frame.brk, typx);
    }

    //NOTE, rhs has to "transform" into lhs
    fn castable(self: Context, lhs: Int, rhs: Int) !bool {
        const dst = self.at(Typx, lhs);
        const src = self.at(Typx, rhs);

        return switch (src) {
            .typx => dst == .typx,
            .noval => dst == .noval,
            .noret => true,
            .undef => dst == .undef,
            .ct_int => switch (dst) {
                .ct_int, .int => true,
                else => false,
            },
            .int => |s| switch (dst) {
                .int => |d| d.sign == s.sign and d.bits >= s.bits,
                else => false,
            },
            else => error.TODO_Castable,
        };
    }

    fn isInteger(self: Context, typx: Int) bool {
        const src = self.at(Typx, typx);

        return switch (src) {
            .ct_int, .int => true,
            else => false,
        };
    }

    fn instanceOf(self: Context, typx: Int, tag: std.meta.Tag(Typx)) bool {
        return self.at(Typx, typx) == tag;
    }

    fn examine(self: *Context, tree: Ast, tokens: Tokens, table: Int, idx: Int) !Int {
        const node = tree.nodes.items[idx];

        errdefer if (error_idx == null) { error_idx = idx ; };

        switch (node.kind) {
            .root => {
                const items = tree.extras(node.extra);

                for (items) |item| {
                    const rdx = try self.examine(tree, tokens, table, item);
                    if (!self.instanceOf(rdx, .noval))
                        return error.IncompatibleTypes;
                }

                return self.add(Typx.NOVAL);
            },
            .fdecl => {
                const pdx = try self.examine(tree, tokens, table, node.extra.fdecl.proto);
                const func = tokens.slice(node.main+1);

                const proto = self.at(Typx, pdx).function;
                const names = self.slice(Int, proto.names, proto.len);
                const items = self.slice(Int, proto.items, proto.len);
                const child = try self.birth(table, idx, .auto, proto.ret, null);

                for (names, items) |ndx, prm| {
                    const name = tokens.slice(ndx);

                    try self.put(child, name, .{
                        .typx = prm,
                        .con = 0,
                    });
                }

                try self.put(table, func, .{
                    .typx = pdx,
                    .con = 0,
                });

                const bdx = try self.examine(tree, tokens, child, node.extra.fdecl.body);
                if (!self.instanceOf(bdx, .noret))
                    return error.IncompatibleTypes;

                return self.add(Typx.NOVAL);
            },
            .fproto => {
                const proto = node.extra.fproto;

                var names = ArrayList(u32).empty;
                var items = ArrayList(u32).empty;

                defer names.deinit(self.allocator);
                defer items.deinit(self.allocator);

                const params = tree.at(proto.prms);
                for (tree.extras(params.extra)) |pdx| {
                    const con = try self.eval(tree, tokens, table, pdx);
                    const tdx = self.at(Constant, con).typx;

                    try names.append(self.allocator, tree.at(pdx).main - 2);
                    try items.append(self.allocator, tdx);
                }

                const con = try self.eval(tree, tokens, table, proto.rtyp);
                const rdx = self.at(Constant, con).typx;

                return self.add(Typx{ .function = .{
                    .names = try self.addSlice(names.items),
                    .items = try self.addSlice(items.items),
                    .len = @intCast(items.items.len),
                    .ret = rdx,
                }});
            },
            .fcall => {
                const call = node.extra.fcall;

                const prdx = try self.examine(tree, tokens, table, call.func);
                const proto = switch (self.at(Typx, prdx)) {
                    .function => |f| f,
                    else => return error.UncallableTerm,
                };

                const list = tree.at(call.args);
                const args = tree.extras(list.extra);
                const prms = self.slice(Int, proto.items, proto.len);

                for (prms, args) |prm, arg| {
                    const adx = try self.examine(tree, tokens, table, arg);

                    if (!try self.castable(prm, adx))
                        return error.IncompatibleTypes;
                }

                return proto.ret;
            },
            .integer => {
                return self.add(Typx.CT_INT);
            },
            .identifier => {
                const name = tokens.slice(node.main);
                const symbol = try self.get(table, name);

                //TODO, update status of linear types

                return symbol.typx;
            },
            .vardef => {
                const name = tokens.slice(node.main+1);

                const lev = try self.eval(tree, tokens, table, node.extra.bin_op.lhs);
                const rev = try self.eval(tree, tokens, table, node.extra.bin_op.rhs);

                const ldx = self.at(Constant, lev).typx;
                const rdx = try self.examine(tree, tokens, table, node.extra.bin_op.rhs);

                if (!try self.castable(ldx, rdx))
                    return error.IncompatibleTypes;

                //TODO, check for linear types in root scope

                try self.put(table, name, .{
                    .typx = ldx,
                    .con = rev,
                });

                return self.add(Typx.NOVAL);
            },
            .block => {
                const child = try self.birth(table, idx, .auto, null, null);
                const stmts = tree.extras(node.extra);
                const end = stmts.len - 1;
                var t = Typx.NOVAL;

                for (stmts, 0..) |stmt, i| {
                    const sdx = try self.examine(tree, tokens, child, stmt);
                    const typx = self.at(Typx, sdx);

                    if (typx == .noret) {
                        if (i < end) return error.EarlyReturn;
                        t = Typx.NORET;
                    }
                }

                //TODO, check for linearity

                return self.add(t);
            },
            .add, .sub, .mul, .div => {
                const ldx = try self.examine(tree, tokens, table, node.extra.bin_op.lhs);
                const rdx = try self.examine(tree, tokens, table, node.extra.bin_op.rhs);

                if (!self.isInteger(ldx) or !self.isInteger(rdx))
                    return error.NonIntegerTerm;

                if (!try self.castable(ldx, rdx))
                    return error.IncompatibleTypes;

                return ldx;
            },
            .ternary => {
                const cdx = try self.examine(tree, tokens, table, node.extra.tri_op.lhs);
                const ldx = try self.examine(tree, tokens, table, node.extra.tri_op.mhs+0);
                const rdx = try self.examine(tree, tokens, table, node.extra.tri_op.mhs+1);

                if (!self.isInteger(cdx))
                    return error.NonIntegerTerm;

                if (!try self.castable(ldx, rdx))
                    return error.IncompatibleTypes;

                return ldx;
            },
            .ret => {
                const rdx = try switch (node.extra.mon_op) {
                    0 => self.add(Typx.NOVAL),
                    else => |v| self.examine(tree, tokens, table, v),
                };

                if (!try self.returnable(table, rdx))
                    return error.UncastableReturn;

                return self.add(Typx.NORET);
            },
            else => return error.TODO_Node,
        }
    }

    fn eval(self: *Context, tree: Ast, tokens: Tokens, table: Int, idx: Int) Error!Int {
        const node = tree.nodes.items[idx];

        errdefer if (error_idx == null) { error_idx = idx ; };

        switch (node.kind) {
            .integer => {
                const text = tokens.slice(node.main);
                const int = try parseInt(i128, text, 0);

                return self.add(Constant{ .int = try BigInt.initSet(self.allocator, int) });
            },
            .identifier => {
                const name = tokens.slice(node.main);
                const symbol = try self.get(table, name);

                return switch (symbol.con) {
                    0 => error.RuntimeEval,
                    else => |c| c,
                };
            },
            .add, .sub, .mul, .div => {
                const lev = self.eval(tree, tokens, table, node.extra.bin_op.lhs) catch |err| return switch (err) {
                    error.RuntimeEval => 0,
                    else => err,
                };

                const rev = self.eval(tree, tokens, table, node.extra.bin_op.rhs) catch |err| return switch (err) {
                    error.RuntimeEval => 0,
                    else => err,
                };

                _ = lev;
                _ = rev;

                //TODO, implement comptime arith
                return 0;
            },
            .ternary => {
                //TODO, implement
                return 0;
            },
            else => {
                std.log.err("eval: {}", .{node.kind});
                return error.TODO_Eval;
            },
        }
    }
};

const Table = struct {
    parent: ?Int,
    symbols: StringHashMap(Symbol),
    storage: Storage,
    frame: Frame,

    fn deinit(self: *Table, gpa: Allocator) void {
        self.symbols.deinit(gpa);
    }
};

const Symbol = struct {
    alive: bool = true,
    typx: Int,
    con: Int,
};

const Storage = enum {
    auto,
    root,
};

const Frame = struct {
    ret: Int,
    brk: Int,

    const root = Frame{
        .ret = 0,
        .brk = 0,
    };
};

const Typx = union(enum) {
    typx: void,
    noval: void,
    noret: void,
    undef: void,
    ct_int: void,
    int: Integer,
    struc: Struct,
    function: Function,

    const Integer = struct {
        bits: Int,
        sign: bool,
    };

    const Struct = struct {
        names: Int,
        items: Int,
        len: Int,
    };

    const Function = struct {
        names: Int,
        items: Int,
        len: Int,
        ret: Vdx,
    };

    const NOVAL = Typx{
        .noval = {},
    };

    const NORET = Typx{
        .noret = {},
    };

    const CT_INT = Typx{
        .ct_int = {},
    };
};

const Constant = union(enum) {
    typx: Int,
    int: BigInt,
    struc: Struct,

    const Struct = struct {
        names: Int,
        items: Int,
        len: Int,
    };

    fn deinit(self: *Constant) void {
        switch (self.*) {
            .int => |*big| big.deinit(),
            else => {},
        }
    }
};

pub fn scan(gpa: Allocator, tree: Ast, tokens: Tokens) !Context {
    var context = try Context.init(gpa);

    const cdx = try context.examine(tree, tokens, 0, 0);
    if (!context.instanceOf(cdx, .noval))
        return error.IncompatibleTypes;

    return context;
}
