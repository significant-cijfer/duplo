const std = @import("std");
const panic = std.debug.panic;
const stringToEnum = std.meta.stringToEnum;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StaticStringMap = std.StaticStringMap;

const State = enum {
    initial,
    integer,
    identifier,
    comment,
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
        @"and",
        @"or",
        @"if",
        @"else",
        @"let",
        @"mut",
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

pub fn lex(gpa: Allocator, source: [:0]const u8) ![]Token {
    var tokens = ArrayList(Token).init(gpa);

    var idx: u32 = 0;
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
            else => |c| panic("Unexpected char: '{c}'\n", .{c}),
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

    return tokens.toOwnedSlice();
}
