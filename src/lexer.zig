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
    idx: u32 = 0,

    pub fn deinit(self: *Tokens) void {
        self.list.deinit(self.allocator);
    }

    pub fn debug(self: Tokens, source: [:0]const u8) void {
        for (self.list.items) |token|
            std.debug.print("token.{s}: '{s}'\n", .{@tagName(token.kind), token.slice(source)});
    }

    pub fn at(self: Tokens, idx: u32) Token {
        return self.list.items[idx];
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

    pub fn slice(self: Token, source: [:0]const u8) []const u8 {
        var idx = self.idx;

        return switch (self.kind) {
            .eof => source[self.idx..],
            .integer => sub: switch (source[idx]) {
                '0'...'9' => {
                    idx += 1;
                    continue :sub source[idx];
                },
                else => {
                    return source[self.idx..idx];
                },
            },
            .identifier => sub: switch (source[idx]) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                    idx += 1;
                    continue :sub source[idx];
                },
                else => {
                    return source[self.idx..idx];
                },
            },
            else => |k| @tagName(k),
        };
    }
};

pub fn lex(gpa: Allocator, source: [:0]const u8) !Tokens {
    var tokens = ArrayList(Token).empty;

    var idx: u32 = 0;
    defer error_idx = idx;

    state: switch (State.initial) {
        .initial => switch (source[idx]) {
            '\n', '\r', '\t', ' ' => {
                idx += 1;
                continue :state .initial;
            },
            0 => {
                try tokens.append(gpa, .{
                    .kind = .eof,
                    .idx = idx,
                });

                idx += 1;
            },
            '+' => {
                try tokens.append(gpa, .{
                    .kind = .@"+",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '-' => {
                try tokens.append(gpa, .{
                    .kind = .@"-",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '*' => {
                try tokens.append(gpa, .{
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
                    try tokens.append(gpa, .{
                        .kind = .@"/",
                        .idx = idx,
                    });

                    idx += 1;
                    continue :state .initial;
                },
            },
            '&' => {
                try tokens.append(gpa, .{
                    .kind = .@"&",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '=' => {
                try tokens.append(gpa, .{
                    .kind = .@"=",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '(' => {
                try tokens.append(gpa, .{
                    .kind = .@"(",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            ')' => {
                try tokens.append(gpa, .{
                    .kind = .@")",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '{' => {
                try tokens.append(gpa, .{
                    .kind = .@"{",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '}' => {
                try tokens.append(gpa, .{
                    .kind = .@"}",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            ':' => {
                try tokens.append(gpa, .{
                    .kind = .@":",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            ';' => {
                try tokens.append(gpa, .{
                    .kind = .@";",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            ',' => {
                try tokens.append(gpa, .{
                    .kind = .@",",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '.' => {
                try tokens.append(gpa, .{
                    .kind = .@".",
                    .idx = idx,
                });

                idx += 1;
                continue :state .initial;
            },
            '0'...'9' => {
                try tokens.append(gpa, .{
                    .kind = .integer,
                    .idx = idx,
                });

                idx += 1;
                continue :state .integer;
            },
            'a'...'z', 'A'...'Z' => {
                try tokens.append(gpa, .{
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
                const token = &tokens.items[tokens.items.len-1];
                const slice = token.slice(source);
                token.kind = stringToEnum(Token.Kind, slice)
                    orelse .identifier;

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

    return .{
        .allocator = gpa,
        .list = tokens,
    };
}
