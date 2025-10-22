const std = @import("std");
const reverse = std.mem.reverse;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lexer = @import("lexer.zig");
const Tokens = Lexer.Tokens;
const Token = Lexer.Token;

pub var error_idx: ?u32 = null;

const Error = error {
    UnexpectedOp,
    UnexpectedToken,
    UnexpectedFirstToken,
}
    || Allocator.Error;

pub const Ast = struct {
    allocator: Allocator,
    nodes: ArrayList(Node),
    extra: ArrayList(u32),

    pub fn deinit(self: *Ast) void {
        self.nodes.deinit(self.allocator);
        self.extra.deinit(self.allocator);
    }

    fn pushNode(self: *Ast, node: Node) !u32 {
        const idx = self.nodes.items.len;
        try self.nodes.append(self.allocator, node);
        return @intCast(idx);
    }

    fn pushExtra(self: *Ast, node: u32) !u32 {
        const idx = self.extra.items.len;
        try self.extra.append(self.allocator, node);
        return @intCast(idx);
    }

    fn pushExtraList(self: *Ast, nodes: []u32) !u32 {
        const idx = self.extra.items.len;
        try self.extra.appendSlice(self.allocator, nodes);
        return @intCast(idx);
    }

    pub fn extras(self: Ast, ext: Node.Extra) []u32 {
        const list = ext.list;
        return self.extra.items[list.idx..list.idx+list.len];
    }

    pub fn debug(
        self: Ast,
        tokens: Tokens,
        source: [:0]const u8,
        idx: u32,
        depth: u32
    ) void {
        const node = self.nodes.items[idx];

        for (0..depth) |_|
            std.debug.print("  ", .{});

        switch (node.kind) {
            .root => std.debug.print("root\n", .{}),
            .fcall => std.debug.print("fcall\n", .{}),
            .fproto => std.debug.print("fproto\n", .{}),
            .block => std.debug.print("{{}}\n", .{}),
            .list => std.debug.print("()\n", .{}),
            else => std.debug.print("{s}\n", .{tokens.at(node.main).slice(source)}),
        }

        switch (node.kind) {
            .root => {
                const roots = self.extras(node.extra);

                for (roots) |root| {
                    self.debug(tokens, source, root, depth+1);
                }
            },
            .fdecl => {
                self.debug(tokens, source, node.extra.fdecl.proto, depth+1);
                self.debug(tokens, source, node.extra.fdecl.body, depth+1);
            },
            .fcall => {
                self.debug(tokens, source, node.extra.fcall.func, depth+1);
                self.debug(tokens, source, node.extra.fcall.args, depth+1);
            },
            .fproto => {
                self.debug(tokens, source, node.extra.fproto.prms, depth+1);
                self.debug(tokens, source, node.extra.fproto.rtyp, depth+1);
            },
            .integer => {},
            .identifier => {},
            .structdef => {
                const mmbrs = self.extras(node.extra);

                for (mmbrs) |mmbr| {
                    self.debug(tokens, source, mmbr, depth+1);
                }
            },
            .structlit => {
                self.debug(tokens, source, node.extra.structlit.head, depth+1);
                self.debug(tokens, source, node.extra.structlit.defs, depth+1);
            },
            .vardef => {
                for (0..depth+1) |_|
                    std.debug.print("  ", .{});

                std.debug.print("{s}\n", .{tokens.at(node.main+1).slice(source)});
                self.debug(tokens, source, node.extra.bin_op.lhs, depth+2);
                self.debug(tokens, source, node.extra.bin_op.rhs, depth+2);
            },
            .block,
            .list => {
                const stmts = self.extras(node.extra);

                for (stmts) |stmt| {
                    self.debug(tokens, source, stmt, depth+1);
                }
            },
            .add,
            .sub,
            .mul,
            .div,
            .assign,
            .logand,
            .logior => {
                self.debug(tokens, source, node.extra.bin_op.lhs, depth+1);
                self.debug(tokens, source, node.extra.bin_op.rhs, depth+1);
            },
            .dot => {
                self.debug(tokens, source, node.extra.mon_op, depth+1);
            },
            .ref,
            .deref => {
                self.debug(tokens, source, node.extra.mon_op, depth+1);
            },
            .ternary => {
                self.debug(tokens, source, node.extra.tri_op.lhs, depth+1);
                self.debug(tokens, source, node.extra.tri_op.mhs, depth+1);
                self.debug(tokens, source, node.extra.tri_op.mhs+1, depth+1);
            },
            .ret => if (node.extra.mon_op != 0) {
                self.debug(tokens, source, node.extra.mon_op, depth+1);
            },
        }
    }
};

const Node = struct {
    main: u32,
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        root, //list
        fdecl, //fdecl
        fcall, //fcall
        fproto, //fproto
        integer, //none
        identifier, //none
        structdef, //list
        structlit, //structlit
        vardef, //bo
        block, //list
        list, //list
        add, //bo
        sub, //bo
        mul, //bo
        div, //bo
        dot, //mo
        ref, //mo
        deref, //mo
        assign, //bo
        logand, //bo
        logior, //bo
        ternary, //to
        ret, //mo
    };

    const Extra = union {
        none: void,
        list: List,
        mon_op: MonOp,
        bin_op: BinOp,
        tri_op: TriOp,
        fdecl: FnDecl,
        fcall: FnCall,
        fproto: FnProto,
        structlit: StructLit,

        const List = struct {
            idx: u32,
            len: u32,
        };

        const MonOp = u32;

        const BinOp = struct {
            lhs: u32,
            rhs: u32,
        };

        const TriOp = struct {
            lhs: u32,
            mhs: u32, //rhs = mhs+1
        };

        const FnDecl = struct {
            proto: u32,
            body: u32,
        };

        const FnCall = struct {
            func: u32,
            args: u32,
        };

        const FnProto = struct {
            prms: u32,
            rtyp: u32,
        };

        const StructLit = struct {
            head: u32,
            defs: u32,
        };
    };
};

const Op = enum {
    add,
    sub,
    mul,
    div,
    ref,
    deref,
    assign,
    logand,
    logior,
    ret,

    const Power = struct {
        lbp: u8,
        rbp: u8,
    };

    fn kind(self: Op) Node.Kind {
        return switch (self) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
            .ref => .ref,
            .deref => .deref,
            .assign => .assign,
            .logand => .logand,
            .logior => .logior,
            .ret => .ret,
        };
    }

    fn prefixPower(self: Op) ?u8 {
        return switch (self) {
            .ref,
            .deref => 10,
            .ret => 1,
            else => null,
        };
    }

    fn infixPower(self: Op) ?Power {
        return switch (self) {
            .assign =>          .{ .lbp = 2, .rbp = 3 },
            .logand, .logior => .{ .lbp = 4, .rbp = 5 },
            .add, .sub =>       .{ .lbp = 6, .rbp = 7 },
            .mul, .div =>       .{ .lbp = 8, .rbp = 9 },
            else => null,
        };
    }
};

pub fn parse(gpa: Allocator, tokens: *Tokens, source: [:0]const u8) !Ast {
    var tree = Ast{
        .allocator = gpa,
        .nodes = .empty,
        .extra = .empty,
    };

    var roots = ArrayList(u32).empty;
    defer roots.deinit(gpa);

    try tree.nodes.append(gpa, undefined);

    while (true) {
        switch (tokens.peek().kind) {
            .eof => break,
            .@"fn" => {
                const odx = tokens.idx;
                try tokens.expect(.@"fn");
                try tokens.expect(.identifier);
                try tokens.expect(.@"(");

                var prms = ArrayList(u32).empty;
                defer prms.deinit(gpa);

                while (true) switch (tokens.peek().kind) {
                    .@")" => {
                        tokens.skip();
                        break;
                    },
                    else => {
                        try tokens.expect(.identifier);
                        try tokens.expect(.@":");
                        const typ = try parseExpr(gpa, tokens, source, &tree, 0);
                        const tnd = try tree.pushNode(typ);
                        try prms.append(gpa, tnd);

                        switch (tokens.next().kind) {
                            .@"," => {},
                            .@")" => break,
                            else => return error.UnexpectedToken,
                        }
                    },
                };

                const rtyp = try parseExpr(gpa, tokens, source, &tree, 0);
                try tokens.expect(.@",");
                const body = try parseExpr(gpa, tokens, source, &tree, 0);
                try tokens.expect(.@";");

                const qrms = try tree.pushNode(.{
                    .main = odx + 2,
                    .kind = .list,
                    .extra = .{ .list = .{
                        .idx = try tree.pushExtraList(prms.items),
                        .len = @intCast(prms.items.len),
                    }},
                });

                const proto = try tree.pushNode(.{
                    .main = odx + 2,
                    .kind = .fproto,
                    .extra = .{ .fproto = .{
                        .prms = qrms,
                        .rtyp = try tree.pushNode(rtyp),
                    }},
                });

                const bdx = try tree.pushNode(body);

                const ndx = try tree.pushNode(.{
                    .main = odx,
                    .kind = .fdecl,
                    .extra = .{ .fdecl = .{
                        .proto = proto,
                        .body = bdx,
                    }},
                });

                try roots.append(gpa, ndx);
            },
            .@"let" => {
                const power = Op.infixPower(.assign).?;

                const odx = tokens.idx;
                try tokens.expect(.@"let");
                try tokens.expect(.identifier);
                try tokens.expect(.@":");
                const typx = try parseExpr(gpa, tokens, source, &tree, power.rbp);
                try tokens.expect(.@"=");
                const expr = try parseExpr(gpa, tokens, source, &tree, 0);
                try tokens.expect(.@";");

                const tdx = try tree.pushNode(typx);
                const edx = try tree.pushNode(expr);

                const ndx = try tree.pushNode(.{
                    .main = odx,
                    .kind = .vardef,
                    .extra = .{ .bin_op = .{
                        .lhs = tdx,
                        .rhs = edx,
                    }},
                });

                try roots.append(gpa, ndx);
            },
            else => return error.UnexpectedToken,
        }
    }

    tree.nodes.items[0] = .{
        .main = @intCast(tokens.list.items.len-1),
        .kind = .root,
        .extra = .{ .list = .{
            .idx = try tree.pushExtraList(roots.items),
            .len = @intCast(roots.items.len),
        }},
    };

    return tree;
}

fn parseExpr(
    gpa: Allocator,
    tokens: *Tokens,
    source: [:0]const u8,
    tree: *Ast,
    bp: u8,
) Error!Node {
    const prelude = try parseExprPrelude(gpa, tokens, source, tree) orelse return error.UnexpectedFirstToken;
    return try parseExprBody(gpa, tokens, source, tree, prelude, bp);
}

fn parseExprPrelude(
    gpa: Allocator,
    tokens: *Tokens,
    source: [:0]const u8,
    tree: *Ast,
) Error!?Node {
    return switch (tokens.next().kind) {
        .integer => .{
            .main = tokens.idx - 1,
            .kind = .integer,
            .extra = .{ .none = {} }
        },
        .identifier => .{
            .main = tokens.idx - 1,
            .kind = .identifier,
            .extra = .{ .none = {} }
        },
        .@"&" => b: {
            const odx = tokens.idx - 1;
            const rbp = Op.prefixPower(.ref).?;
            const rhs = try parseExpr(gpa, tokens, source, tree, rbp);
            const rnd = try tree.pushNode(rhs);

            break :b .{
                .main = odx,
                .kind = .ref,
                .extra = .{ .mon_op = rnd },
            };
        },
        .@"*" => b: {
            const odx = tokens.idx - 1;
            const rbp = Op.prefixPower(.deref).?;
            const rhs = try parseExpr(gpa, tokens, source, tree, rbp);
            const rnd = try tree.pushNode(rhs);

            break :b .{
                .main = odx,
                .kind = .deref,
                .extra = .{ .mon_op = rnd },
            };
        },
        .@"{" => b: {
            const odx = tokens.idx - 1;
            var elems = ArrayList(u32).empty;
            defer elems.deinit(gpa);

            while (true) switch (tokens.peek().kind) {
                .@"}" => {
                    tokens.skip();
                    break;
                },
                .@"let" => {
                    const power = Op.infixPower(.assign).?;

                    const ldx = tokens.idx;
                    try tokens.expect(.@"let");
                    try tokens.expect(.identifier);
                    try tokens.expect(.@":");
                    const typx = try parseExpr(gpa, tokens, source, tree, power.rbp);
                    try tokens.expect(.@"=");
                    const expr = try parseExpr(gpa, tokens, source, tree, 0);
                    try tokens.expect(.@";");

                    const tdx = try tree.pushNode(typx);
                    const edx = try tree.pushNode(expr);

                    const ndx = try tree.pushNode(.{
                        .main = ldx,
                        .kind = .vardef,
                        .extra = .{ .bin_op = .{
                            .lhs = tdx,
                            .rhs = edx,
                        }},
                    });

                    try elems.append(gpa, ndx);
                },
                else => {
                    const rhs = try parseExpr(gpa, tokens, source, tree, 0);
                    try tokens.expect(.@";");

                    const rnd = try tree.pushNode(rhs);
                    try elems.append(gpa, rnd);
                },
            };

            break :b .{
                .main = odx,
                .kind = .block,
                .extra = .{ .list = .{
                    .idx = try tree.pushExtraList(elems.items),
                    .len = @intCast(elems.items.len),
                }},
            };
        },
        .@"return" => b: {
            const odx = tokens.idx - 1;
            const rbp = Op.prefixPower(.ret).?;

            const rnd = if (try parseExprPrelude(gpa, tokens, source, tree)) |pre| r: {
                const rhs = try parseExprBody(gpa, tokens, source, tree, pre, rbp);
                break :r try tree.pushNode(rhs);
            } else r: {
                break :r 0;
            };

            break :b .{
                .main = odx,
                .kind = .ret,
                .extra = .{ .mon_op = rnd },
            };
        },
        .@"struct" => b: {
            const odx = tokens.idx - 1;
            var members = ArrayList(u32).empty;
            defer members.deinit(gpa);

            tokens.expect(.@"linear") catch {};
            try tokens.expect(.@"{");

            while (true) switch (tokens.peek().kind) {
                .@"}" => {
                    tokens.skip();
                    break;
                },
                else => {
                    try tokens.expect(.identifier);
                    try tokens.expect(.@":");
                    const typ = try parseExpr(gpa, tokens, source, tree, 0);
                    const tnd = try tree.pushNode(typ);
                    try members.append(gpa, tnd);

                    switch (tokens.next().kind) {
                        .@"," => {},
                        .@"}" => break,
                        else => return error.UnexpectedToken,
                    }
                },
            };

            break :b .{
                .main = odx,
                .kind = .structdef,
                .extra = .{ .list = .{
                    .idx = try tree.pushExtraList(members.items),
                    .len = @intCast(members.items.len),
                }},
            };
        },
        .@"if" => b: {
            const odx = tokens.idx - 1;
            const chs = try parseExpr(gpa, tokens, source, tree, 0);
            const lhs = try parseExpr(gpa, tokens, source, tree, 0);
            try tokens.expect(.@"else");
            const rhs = try parseExpr(gpa, tokens, source, tree, 0);

            const cnd = try tree.pushNode(chs);
            const lnd = try tree.pushNode(lhs);
            const rnd = try tree.pushNode(rhs);
            _ = rnd;

            break :b .{
                .main = odx,
                .kind = .ternary,
                .extra = .{ .tri_op = .{
                    .lhs = cnd,
                    .mhs = lnd,
                }},
            };
        },
        else => {
            tokens.idx -= 1;
            return null;
        },
    };

}

fn parseExprBody(
    gpa: Allocator,
    tokens: *Tokens,
    source: [:0]const u8,
    tree: *Ast,
    pre: Node,
    bp: u8,
) Error!Node {
    var lhs = pre;

    while (true) {
        const odx = tokens.idx;
        const op: Op = switch (tokens.peek().kind) {
            .@"+" => .add,
            .@"-" => .sub,
            .@"*" => .mul,
            .@"/" => .div,
            .@"=" => .assign,
            .@"and" => .logand,
            .@"or" => .logior,
            .@"(" => {
                var args = ArrayList(u32).empty;
                defer args.deinit(gpa);
                tokens.skip();

                while (true) switch (tokens.peek().kind) {
                    .@"," => {
                        tokens.skip();
                        try args.append(gpa, 0);
                    },
                    .@")" => {
                        tokens.skip();
                        break;
                    },
                    else => {
                        const arg = try parseExpr(gpa, tokens, source, tree, 0);
                        const gnd = try tree.pushNode(arg);
                        try args.append(gpa, gnd);

                        switch (tokens.next().kind) {
                            .@"," => {},
                            .@")" => break,
                            else => return error.UnexpectedToken,
                        }
                    },
                };

                const lnd = try tree.pushNode(lhs);

                const list = try tree.pushNode(.{
                    .main = odx,
                    .kind = .list,
                    .extra = .{ .list = .{
                        .idx = try tree.pushExtraList(args.items),
                        .len = @intCast(args.items.len),
                    }},
                });

                lhs = .{
                    .main = odx,
                    .kind = .fcall,
                    .extra = .{ .fcall = .{
                        .func = lnd,
                        .args = list,
                    }},
                };

                continue;
            },
            .@"{" => {
                var defs = ArrayList(u32).empty;
                defer defs.deinit(gpa);
                tokens.skip();

                while (true) switch (tokens.peek().kind) {
                    .@"}" => {
                        tokens.skip();
                        break;
                    },
                    else => {
                        try tokens.expect(.@".");
                        try tokens.expect(.identifier);
                        try tokens.expect(.@"=");
                        const def = try parseExpr(gpa, tokens, source, tree, 0);
                        const dnd = try tree.pushNode(def);
                        try defs.append(gpa, dnd);

                        switch (tokens.next().kind) {
                            .@"," => {},
                            .@"}" => break,
                            else => return error.UnexpectedToken,
                        }
                    },
                };

                const lnd = try tree.pushNode(lhs);

                const list = try tree.pushNode(.{
                    .main = odx,
                    .kind = .list,
                    .extra = .{ .list = .{
                        .idx = try tree.pushExtraList(defs.items),
                        .len = @intCast(defs.items.len),
                    }},
                });

                lhs = .{
                    .main = odx,
                    .kind = .structlit,
                    .extra = .{ .structlit = .{
                        .head = lnd,
                        .defs = list,
                    }},
                };

                continue;
            },
            .@"." => {
                tokens.skip();
                try tokens.expect(.identifier);

                const lnd = try tree.pushNode(lhs);

                lhs = .{
                    .main = odx,
                    .kind = .dot,
                    .extra = .{ .mon_op = lnd },
                };

                continue;
            },
            else => break,
        };

        if (op.infixPower()) |p| {
            if (p.lbp < bp)
                break;

            tokens.skip();

            const lnd = try tree.pushNode(lhs);
            const rhs = try parseExpr(gpa, tokens, source, tree, p.rbp);
            const rnd = try tree.pushNode(rhs);

            lhs = .{
                .main = odx,
                .kind = op.kind(),
                .extra = .{ .bin_op = .{
                    .lhs = lnd,
                    .rhs = rnd,
                }},
            };

            continue;
        }

        return error.UnexpectedOp;
    }

    return lhs;
}
