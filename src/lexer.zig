const std = @import("std");
const stringToEnum = std.meta.stringToEnum;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StaticStringMap = std.StaticStringMap;

const Parser = @import("parser.zig");

pub var error_idx: ?u32 = null;

const State = enum {
    initial,
    integer,
    identifier,
    comment,
};

pub const Tokens = struct {
    allocator: Allocator,
    list: ArrayList(Token),
    source: [:0]const u8,
    idx: u32 = 0,

    pub fn deinit(self: *Tokens) void {
        self.list.deinit(self.allocator);
    }

    pub fn debug(self: Tokens) void {
        for (self.list.items, 0..) |token, idx|
            std.debug.print("token.{s}: '{s}'\n", .{@tagName(token.kind), self.slice(@intCast(idx))});
    }

    pub fn at(self: Tokens, idx: u32) Token {
        return self.list.items[idx];
    }

    pub fn append(self: *Tokens, token: Token) !void {
        try self.list.append(self.allocator, token);
    }

    pub fn slice(self: Tokens, tdx: u32) []const u8 {
        const token = self.list.items[tdx];
        var idx = token.idx;

        return switch (token.kind) {
            .eof => self.source[token.idx..],
            .integer => sub: switch (self.source[idx]) {
                '0'...'9' => {
                    idx += 1;
                    continue :sub self.source[idx];
                },
                else => {
                    return self.source[token.idx..idx];
                },
            },
            .identifier => sub: switch (self.source[idx]) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                    idx += 1;
                    continue :sub self.source[idx];
                },
                else => {
                    return self.source[token.idx..idx];
                },
            },
            else => |k| @tagName(k),
        };
    }

    pub fn peek(self: Tokens) Token {
        Parser.error_idx = self.idx; //NOTE, might not be the correct place to put this
        return self.list.items[self.idx];
    }

    pub fn next(self: *Tokens) Token {
        const token = self.peek();
        self.idx += 1;
        return token;
    }

    pub fn skip(self: *Tokens) void {
        _ = self.next();
    }

    pub fn expect(self: *Tokens, kind: Token.Kind) !void {
        if (self.peek().kind != kind)
            return error.UnexpectedToken;
        self.skip();
    }
};

pub const Token = struct {
    kind: Kind,
    idx: u32,

    pub const Kind = enum {
        eof,
        integer,
        identifier,
        @"+",
        @"-",
        @"*",
        @"/",
        @"&",
        @"=",
        @"(",
        @")",
        @"{",
        @"}",
        @":",
        @";",
        @",",
        @".",
        @"fn",
        @"destroy",
        @"return",
        @"struct",
        @"linear",
        @"and",
        @"or",
        @"if",
        @"else",
        @"let",
        @"var",
    };
};

pub fn lex(gpa: Allocator, source: [:0]const u8) !Tokens {
    var tokens = Tokens{
        .allocator = gpa,
        .list = .empty,
        .source = source,
    };

    var idx: u32 = 0;
    defer error_idx = idx;

    state: switch (State.initial) {
        .initial => switch (source[idx]) {
            '\n', '\r', '\t', ' ' => {
                idx += 1;
                continue :state .initial;
            },
            0 => {
                try tokens.append(.{
                    .kind = .eof,
                    .idx = idx,
                });

                idx += 1;
            },
            '+' => {
                try tokens.append(.{
                    .kind = .@"+",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '-' => {
                try tokens.append(.{
                    .kind = .@"-",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '*' => {
                try tokens.append(.{
                    .kind = .@"*",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '/' => switch (source[idx+1]) {
                '/' => {
                    continue :state .comment;
                },
                else => {
                    try tokens.append(.{
                        .kind = .@"/",
                        .idx = idx,
                    });

                    idx += 1;
                    continue :state .initial;
                },
            },
            '&' => {
                try tokens.append(.{
                    .kind = .@"&",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '=' => {
                try tokens.append(.{
                    .kind = .@"=",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '(' => {
                try tokens.append(.{
                    .kind = .@"(",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            ')' => {
                try tokens.append(.{
                    .kind = .@")",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '{' => {
                try tokens.append(.{
                    .kind = .@"{",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '}' => {
                try tokens.append(.{
                    .kind = .@"}",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            ':' => {
                try tokens.append(.{
                    .kind = .@":",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            ';' => {
                try tokens.append(.{
                    .kind = .@";",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            ',' => {
                try tokens.append(.{
                    .kind = .@",",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '.' => {
                try tokens.append(.{
                    .kind = .@".",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '0'...'9' => {
                try tokens.append(.{
                    .kind = .integer,
                    .idx = idx,
                });

                idx += 1;
                continue :state .integer;
            },
            'a'...'z', 'A'...'Z' => {
                try tokens.append(.{
                    .kind = .identifier,
                    .idx = idx,
                });

                idx += 1;
                continue :state .identifier;
            },
            else => return error.UnexpectedChar,
        },
        .integer => sub: switch (source[idx]) {
            '0'...'9' => {
                idx += 1;
                continue :sub source[idx];
            },
            else => continue :state .initial,
        },
        .identifier => sub: switch (source[idx]) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                idx += 1;
                continue :sub source[idx];
            },
            else => {
                //NOTE, litte ugly blob, sadly this'll have to stay
                const last: u32 = @intCast(tokens.list.items.len-1);
                const slice = tokens.slice(last);
                const kind = stringToEnum(Token.Kind, slice) orelse .identifier;
                tokens.list.items[last].kind = kind;

                continue :state .initial;
            },
        },
        .comment => sub: switch (source[idx]) {
            0, '\n' => {
                continue :state .initial;
            },
            else => {
                idx += 1;
                continue :sub source[idx];
            },
        },
    }

    return tokens;
}
