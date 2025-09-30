const std = @import("std");
const panic = std.debug.panic;
const reverse = std.mem.reverse;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;

const Error = error {
    UnexpectedToken,
    UnexpectedFirstToken,
}
    || Allocator.Error
    || std.fmt.ParseIntError;

pub const Ast = struct {
    nodes: ArrayList(Node),
    extra: ArrayList(u32),

    pub fn extraSlice(self: Ast, idx: u32) []u32 {
        const len = self.extra.items[idx];
        return self.extra.items[idx+1..idx+len+1];
    }

    pub fn deinit(self: Ast) void {
        self.nodes.deinit();
        self.extra.deinit();
    }

    fn pushNode(self: *Ast, node: Node) !u32 {
        const idx = self.nodes.items.len;
        try self.nodes.append(node);
        return @intCast(idx);
    }

    fn pushExtra(self: *Ast, node: u32) !u32 {
        const idx = self.extra.items.len;
        try self.extra.append(node);
        return @intCast(idx);
    }

    fn pushExtraList(self: *Ast, nodes: []u32) !u32 {
        const idx = self.extra.items.len;
        try self.extra.append(@intCast(nodes.len));
        try self.extra.appendSlice(nodes);
        return @intCast(idx);
    }

    pub fn debug(
        self: Ast,
        tokens: []const Token,
        source: [:0]const u8,
        idx: u32,
        depth: u32
    ) void {
        const node = self.nodes.items[idx];

        for (0..depth) |_|
            std.debug.print("  ", .{});

        switch (node.kind) {
            .root =>
                std.debug.print("root\n", .{}),
            else =>
                std.debug.print("{s}\n", .{tokens[node.main].slice(source)}),
        }

        switch (node.kind) {
            .root,
            .block => {
                const slice = self.extraSlice(node.extra.lhs);
                for (slice) |ndx| {
                    self.debug(tokens, source, ndx, depth+1);
                }
            },
            .integer,
            .identifier => {},
            .structdef => @panic("TODO(debug), implement debug for structdef"),
            .ref,
            .deref => {
                self.debug(tokens, source, node.extra.lhs, depth+1);
            },
            .dot => {
                self.debug(tokens, source, node.extra.lhs, depth+1);
                for (0..depth+1) |_|
                    std.debug.print("  ", .{});
                std.debug.print("{s}\n", .{tokens[node.main+1].slice(source)});
            },
            .add,
            .sub,
            .mul,
            .div,
            .assign,
            .logand,
            .logior => {
                self.debug(tokens, source, node.extra.lhs, depth+1);
                self.debug(tokens, source, node.extra.rhs, depth+1);
            },
            .ternary => {
                self.debug(tokens, source, node.extra.lhs, depth+1);
                self.debug(tokens, source, node.extra.rhs+0, depth+1);
                self.debug(tokens, source, node.extra.rhs+1, depth+1);
            },
            .fn_decl => {
                self.debug(tokens, source, node.extra.lhs+1, depth+1);
                self.debug(tokens, source, node.extra.lhs+0, depth+1);
            },
            .fn_call => {
                self.debug(tokens, source, node.extra.lhs, depth+1);

                const slice = self.extraSlice(node.extra.rhs);
                for (slice) |ndx|
                    self.debug(tokens, source, ndx, depth+2);
            },
            .ret => {
                if (node.extra.lhs != 0)
                    self.debug(tokens, source, node.extra.lhs, depth+1);
            },
        }
    }
};

const Node = struct {
    main: u32,
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        root,
        fn_decl,
        fn_call,
        integer,
        identifier,
        structdef,
        block,
        add,
        sub,
        mul,
        div,
        dot,
        ref,
        deref,
        assign,
        logand,
        logior,
        ternary,
        ret,
    };

    const Extra = struct {
        lhs: u32,
        rhs: u32
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

    //TODO(infixPower), reorganize and standardize
    fn infixPower(self: Op) ?Power {
        return switch (self) {
            .assign => .{ .lbp = 2, .rbp = 3 },
            .logand, .logior => .{ .lbp = 4, .rbp = 5 },
            .add, .sub => .{ .lbp = 6, .rbp = 7 },
            .mul, .div => .{ .lbp = 8, .rbp = 9 },
            else => null,
        };
    }
};

fn peek(tokens: []const Token, idx: *const u32) Token {
    return tokens[idx.*];
}

fn next(tokens: []const Token, idx: *u32) Token {
    const token = tokens[idx.*];
    idx.* += 1;
    return token;
}

fn skip(tokens: []const Token, idx: *u32) void {
    switch (tokens[idx.*].kind) {
        .eof => {},
        else => idx.* += 1,
    }
}

fn expect(tokens: []const Token, idx: *u32, kind: Token.Kind) !void {
    if (peek(tokens, idx).kind != kind)
        return error.UnexpectedToken;
    skip(tokens, idx);
}

pub fn parse(gpa: Allocator, tokens: []const Token, source: [:0]const u8) !Ast {
    var tree = Ast{
        .nodes = .init(gpa),
        .extra = .init(gpa),
    };

    var roots = ArrayList(u32).init(gpa);
    defer roots.deinit();

    //TODO(parse), replace root with struct
    try tree.nodes.append(.{
        .main = undefined,
        .kind = .root,
        .extra = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });

    var idx: u32 = 0;

    while (true) {
        switch (peek(tokens, &idx).kind) {
            .eof => break,
            .@"fn" => {
                const odx = idx;
                try expect(tokens, &idx, .@"fn");
                try expect(tokens, &idx, .identifier);
                try expect(tokens, &idx, .@"(");

                var prms = ArrayList(u32).init(gpa);
                defer prms.deinit();

                while (true) switch (peek(tokens, &idx).kind) {
                    .@")" => {
                        skip(tokens, &idx);
                        break;
                    },
                    else => {
                        try expect(tokens, &idx, .identifier);
                        try expect(tokens, &idx, .@":");
                        const typ = try parseExpr(gpa, tokens, source, &tree, &idx, 0);
                        const tnd = try tree.pushNode(typ);
                        try prms.append(tnd);

                        switch (next(tokens, &idx).kind) {
                            .@"," => {},
                            .@")" => break,
                            else => return error.UnexpectedToken,
                        }
                    },
                };

                const rtyp = try parseExpr(gpa, tokens, source, &tree, &idx, 0);
                const body = try parseExpr(gpa, tokens, source, &tree, &idx, 0);
                try expect(tokens, &idx, .@";");

                const bdx = try tree.pushNode(body);
                const rdx = try tree.pushNode(rtyp);
                _ = rdx;

                const tdx = try tree.pushExtra(0); //NOTE, used to be table
                const pdx = try tree.pushExtraList(prms.items); //TODO(parse), add new function pushExtraProto
                _ = pdx;

                const ndx = try tree.pushNode(.{
                    .main = odx,
                    .kind = .fn_decl,
                    .extra = .{
                        .lhs = bdx,
                        .rhs = tdx,
                    },
                });
                try roots.append(ndx);
            },
            //.@"let" => {
            //    const table = 0;
            //    const power = Op.infixPower(.assign).?;

            //    const ldx = idx;
            //    try expect(tokens, &idx, .@"let");
            //    try expect(tokens, &idx, .identifier);
            //    try expect(tokens, &idx, .@":");
            //    const typ = try parseExpr(gpa, tokens, source, &idx, &nodes, &extra, power.rbp);
            //    try expect(tokens, &idx, .@"=");
            //    const expr = try parseExpr(gpa, tokens, source, &idx, &nodes, &extra, 0);
            //    try expect(tokens, &idx, .@";");

            //    _ = ldx;

            //    const tdx = try pushNode(&nodes, typ);
            //    const edx = try pushNode(&nodes, expr);

            //    const tree = Ast{
            //        .nodes = nodes.items,
            //        .extra = extra.items,
            //    };

            //    const name = tokens[typ.main-2].slice(source);
            //    const tv = try tree.eval(tokens, source, tables, null, .Type, table, tdx);
            //    const ev = try tree.eval(tokens, source, tables, name, tv.typ, table, edx);

            //    try tables.put(table, name, .{
            //        .storage = .public,
            //        .value = ev,
            //        .typ = tv.typ
            //    });
            //},
            else => return error.UnexpectedToken,
        }
    }

    const edx = try tree.pushExtraList(roots.items);
    tree.nodes.items[0].kind = .root;
    tree.nodes.items[0].extra.lhs = edx;
    tree.nodes.items[0].extra.rhs = undefined;


    return tree;
}

fn parseExpr(
    gpa: Allocator,
    tokens: []const Token,
    source: [:0]const u8,
    tree: *Ast,
    idx: *u32,
    bp: u8,
) Error!Node {
    const prelude = try parseExprPrelude(gpa, tokens, source, tree, idx) orelse return error.UnexpectedFirstToken;
    return try parseExprBody(gpa, tokens, source, tree, prelude, idx, bp);
}

fn parseExprPrelude(
    gpa: Allocator,
    tokens: []const Token,
    source: [:0]const u8,
    tree: *Ast,
    idx: *u32,
) Error!?Node {
    return switch (next(tokens, idx).kind) {
        .integer => .{
            .main = idx.* - 1,
            .kind = .integer,
            .extra = undefined
        },
        .identifier => .{
            .main = idx.* - 1,
            .kind = .identifier,
            .extra = undefined
        },
        .@"&" => b: {
            const odx = idx.* - 1;
            const rbp = Op.prefixPower(.ref).?;
            const rhs = try parseExpr(gpa, tokens, source, tree, idx, rbp);
            const rnd = try tree.pushNode(rhs);

            break :b .{
                .main = odx,
                .kind = .ref,
                .extra = .{
                    .lhs = rnd,
                    .rhs = undefined,
                },
            };
        },
        .@"*" => b: {
            const odx = idx.* - 1;
            const rbp = Op.prefixPower(.deref).?;
            const rhs = try parseExpr(gpa, tokens, source, tree, idx, rbp);
            const rnd = try tree.pushNode(rhs);

            break :b .{
                .main = odx,
                .kind = .deref,
                .extra = .{
                    .lhs = rnd,
                    .rhs = undefined,
                },
            };
        },
        .@"{" => b: {
            const odx = idx.* - 1;
            var elems = ArrayList(u32).init(gpa);
            defer elems.deinit();

            while (true) switch (peek(tokens, idx).kind) {
                .@"}" => {
                    skip(tokens, idx);
                    break;
                },
                //.@"let" => {
                //    const power = Op.infixPower(.assign).?;

                //    const ldx = idx.*;
                //    try expect(tokens, idx, .@"let");
                //    try expect(tokens, idx, .identifier);
                //    try expect(tokens, idx, .@":");
                //    const typ = try parseExpr(gpa, tokens, source, idx, nodes, extra, power.rbp);
                //    try expect(tokens, idx, .@"=");
                //    const expr = try parseExpr(gpa, tokens, source, idx, nodes, extra, 0);
                //    try expect(tokens, idx, .@";");

                //    const tnd = try pushNode(nodes, typ);
                //    const ind = try pushNode(nodes, .{
                //        .main = ldx + 1,
                //        .kind = .identifier,
                //        .extra = undefined,
                //    });
                //    const end = try pushNode(nodes, expr);
                //    const mnd = try pushNode(nodes, .{
                //        .main = ldx,
                //        .kind = .assign,
                //        .extra = .{
                //            .lhs = ind,
                //            .rhs = end,
                //        },
                //    });

                //    try elems.append(mnd);

                //    const tree = Ast{
                //        .nodes = nodes.items,
                //        .extra = extra.items,
                //    };

                //    const name = tokens[typ.main-2].slice(source);
                //    const tv = try tree.eval(tokens, source, tables, null, .Type, table, tnd);
                //    const ev = try tree.eval(tokens, source, tables, name, tv.typ, table, end);

                //    try tables.put(table, name, .{
                //        .storage = .auto,
                //        .value = ev,
                //        .typ = tv.typ,
                //    });
                //},
                else => {
                    const rhs = try parseExpr(gpa, tokens, source, tree, idx, 0);
                    try expect(tokens, idx, .@";");

                    const rnd = try tree.pushNode(rhs);
                    try elems.append(rnd);
                },
            };

            const edx = try tree.pushExtraList(elems.items);

            break :b .{
                .main = odx,
                .kind = .block,
                .extra = .{
                    .lhs = edx,
                    .rhs = 0, //NOTE, used to be table
                },
            };
        },
        .@"return" => b: {
            const odx = idx.* - 1;
            const rbp = Op.prefixPower(.ret).?;

            const rnd = if (try parseExprPrelude(gpa, tokens, source, tree, idx)) |pre| r: {
                const rhs = try parseExprBody(gpa, tokens, source, tree, pre, idx, rbp);
                break :r try tree.pushNode(rhs);
            } else r: {
                break :r 0;
            };

            break :b .{
                .main = odx,
                .kind = .ret,
                .extra = .{
                    .lhs = rnd,
                    .rhs = undefined,
                },
            };
        },
        .@"struct" => b: {
            const odx = idx.* - 1;
            var members = ArrayList(u32).init(gpa);
            defer members.deinit();

            try expect(tokens, idx, .@"{");

            while (true) switch (peek(tokens, idx).kind) {
                .@"}" => {
                    skip(tokens, idx);
                    break;
                },
                else => {
                    try expect(tokens, idx, .identifier);
                    try expect(tokens, idx, .@":");
                    const typ = try parseExpr(gpa, tokens, source, tree, idx, 0);
                    const tnd = try tree.pushNode(typ);
                    try members.append(tnd);

                    switch (next(tokens, idx).kind) {
                        .@"," => {},
                        .@"}" => break,
                        else => return error.UnexpectedToken,
                    }
                },
            };

            const mnd = try tree.pushExtraList(members.items);

            break :b .{
                .main = odx,
                .kind = .structdef,
                .extra = .{
                    .lhs = mnd,
                    .rhs = undefined,
                },
            };
        },
        .@"if" => b: {
            const odx = idx.* - 1;
            const chs = try parseExpr(gpa, tokens, source, tree, idx, 0);
            const lhs = try parseExpr(gpa, tokens, source, tree, idx, 0);
            try expect(tokens, idx, .@"else");
            const rhs = try parseExpr(gpa, tokens, source, tree, idx, 0);

            const cnd = try tree.pushNode(chs);
            const lnd = try tree.pushNode(lhs);
            const rnd = try tree.pushNode(rhs);
            _ = rnd;

            break :b .{
                .main = odx,
                .kind = .ternary,
                .extra = .{
                    .lhs = cnd,
                    .rhs = lnd,
                },
            };
        },
        else => {
            idx.* -= 1;
            return null;
        },
    };

}

fn parseExprBody(
    gpa: Allocator,
    tokens: []const Token,
    source: [:0]const u8,
    tree: *Ast,
    pre: Node,
    idx: *u32,
    bp: u8,
) Error!Node {
    var lhs = pre;

    while (true) {
        const odx = idx.*;
        const op: Op = switch (peek(tokens, idx).kind) {
            .@"+" => .add,
            .@"-" => .sub,
            .@"*" => .mul,
            .@"/" => .div,
            .@"=" => .assign,
            .@"and" => .logand,
            .@"or" => .logior,
            .@"(" => {
                var args = ArrayList(u32).init(gpa);
                defer args.deinit();
                skip(tokens, idx);

                while (true) switch (peek(tokens, idx).kind) {
                    .@"," => {
                        skip(tokens, idx);
                        try args.append(0);
                    },
                    .@")" => {
                        skip(tokens, idx);
                        break;
                    },
                    else => {
                        const arg = try parseExpr(gpa, tokens, source, tree, idx, 0);
                        const gnd = try tree.pushNode(arg);
                        try args.append(gnd);

                        switch (next(tokens, idx).kind) {
                            .@"," => {},
                            .@")" => break,
                            else => return error.UnexpectedToken,
                        }
                    },
                };

                const lnd = try tree.pushNode(lhs);
                const gnd = try tree.pushExtraList(args.items);

                lhs = .{
                    .main = odx,
                    .kind = .fn_call,
                    .extra = .{
                        .lhs = lnd,
                        .rhs = gnd,
                    },
                };

                continue;
            },
            .@"." => {
                skip(tokens, idx);
                try expect(tokens, idx, .identifier);

                const lnd = try tree.pushNode(lhs);

                lhs = .{
                    .main = odx,
                    .kind = .dot,
                    .extra = .{
                        .lhs = lnd,
                        .rhs = undefined,
                    },
                };

                continue;
            },
            else => break,
        };

        if (op.infixPower()) |p| {
            if (p.lbp < bp)
                break;

            skip(tokens, idx);

            const lnd = try tree.pushNode(lhs);
            const rhs = try parseExpr(gpa, tokens, source, tree, idx, p.rbp);
            const rnd = try tree.pushNode(rhs);

            lhs = .{
                .main = odx,
                .kind = op.kind(),
                .extra = .{
                    .lhs = lnd,
                    .rhs = rnd,
                },
            };

            continue;
        }

        panic("Unhandled op: {}", .{op});
    }

    return lhs;
}
