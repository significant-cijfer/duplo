const std = @import("std");
const lego = @import("lego");

const parseInt = std.fmt.parseInt;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMapUnmanaged;
const Managed = std.math.big.int.Managed;

const Graph = lego.Graph;
const Function = lego.Function;
const Location = lego.Location;
const Constant = lego.Constant;
const Block = lego.Block;
const Inst = lego.Inst;
const Typx = lego.Typx;

const BigInt = lego.BigInt;
const LocationList = lego.LocationList;
const LocationExtraList = lego.LocationExtraList;

const Lexer = @import("lexer.zig");
const Tokens = Lexer.Tokens;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

const Analyzer = @import("analyzer.zig");
const Context = Analyzer.Context;
const Symbol = Analyzer.Symbol;
const ATypx = Analyzer.Typx;

const Int = u32;
const Vdx = u32;

pub var error_idx: ?Int = null;

const Builder = struct {
    allocator: Allocator,
    functions: ArrayList(Function),
    locations: ArrayList(Location),
    constants: ArrayList(Constant),
    manageds: ArrayList(Managed),
    strings: ArrayList([]const u8),
    blocks: ArrayList(Block),
    insts: ArrayList(Inst),
    typxs: ArrayList(Typx),

    local: ArrayList(Int),
    root: Root,

    const Root = struct {
        varbs: StringHashMap(Int),
    };

    const State = struct {
        Vdx, // location
        Int, // block
    };

    pub fn deinit(self: *Builder) void {
        for (self.manageds.items) |*managed|
            managed.deinit();

        self.functions.deinit(self.allocator);
        self.locations.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.manageds.deinit(self.allocator);
        self.strings.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.insts.deinit(self.allocator);
        self.typxs.deinit(self.allocator);
        self.local.deinit(self.allocator);

        self.root.varbs.deinit(self.allocator);
    }

    fn listOf(self: Builder, comptime T: type) []const T {
        return switch (T) {
            Function => self.functions.items,
            Location => self.locations.items,
            Constant => self.constants.items,
            Managed => self.manages.items,
            []const u8 => self.strings.items,
            Block => self.blocks.items,
            Inst => self.insts.items,
            Typx => self.typxs.items,
            Int => self.local.items,
            else => @compileError("No list exists of type: " ++ @typeName(T)),
        };
    }

    fn arrayOf(self: *Builder, comptime T: type) *ArrayList(T) {
        return switch (T) {
            Function => &self.functions,
            Location => &self.locations,
            Constant => &self.constants,
            Managed => &self.manageds,
            []const u8 => &self.strings,
            Block => &self.blocks,
            Inst => &self.insts,
            Typx => &self.typxs,
            Int => &self.local,
            else => @compileError("No list exists of type: " ++ @typeName(T)),
        };
    }

    pub fn at(self: Builder, comptime T: type, idx: Int) T {
        return self.listOf(T)[idx];
    }

    pub fn slice(self: Builder, comptime T: type, idx: Int, len: Int) []const T {
        return self.listOf(T)[idx..idx+len];
    }

    fn add(self: *Builder, value: anytype) !Int {
        const arr = self.arrayOf(@TypeOf(value));
        const idx = arr.items.len;

        try arr.append(self.allocator, value);
        return @intCast(idx);
    }

    fn addSlice(self: *Builder, value: anytype) !Int {
        const arr = self.arrayOf(std.meta.Child(@TypeOf(value)));
        const idx = arr.items.len;

        try arr.appendSlice(self.allocator, value);
        return @intCast(idx);
    }

    fn newSlice(self: *Builder, comptime T: type, len: Int) !struct { Int, []T } {
        const arr = self.arrayOf(T);
        const idx = arr.items.len;

        return .{
            @intCast(idx),
            try arr.addManyAsSlice(self.allocator, len),
        };
    }

    fn newBlock(self: *Builder) !Int {
        return self.add(Block{
            .idx = @intCast(self.insts.items.len),
            .len = undefined,
            .flow = undefined,
        });
    }

    fn finishBlock(self: *Builder, block: Int, flow: Block.Flow) void {
        const ptr = &self.blocks.items[block];
        const idx = ptr.idx;
        const last = block == self.blocks.items.len-1;

        const end = switch (last) {
            true => self.insts.items.len,
            false => self.blocks.items[block+1].idx,
        };

        ptr.len = @as(Int, @intCast(end)) - idx;
        ptr.flow = flow;
    }

    fn trivialize(self: *Builder, comptime T: type, ctx: Context, idx: Int) !T {
        _ = self;

        switch (T) {
            Typx => {
                return switch (ctx.at(ATypx, idx)) {
                    .int => |i| .{ .primitive = .{
                        .bits = i.bits,
                        .sign = i.sign,
                    }},
                    .ct_int,
                    .function => .{ .word = {} },
                    else => |t| std.debug.panic("TODO, handle trivialize for: {s}", .{ @tagName(t) }),
                };
            },
            else => @compileError("No list exists of type: " ++ @typeName(T)),
        }
    }

    fn drive(self: *Builder) !LocationList {
        const idx = self.locations.items.len;
        const len = self.local.items.len;

        for (self.local.items) |item|
            _ = try self.add(self.at(Location, item));

        self.local.clearRetainingCapacity();

        return .{
            .items = @intCast(idx),
            .len = @intCast(len),
        };
    }

    fn auto(self: *Builder, typx: Int) !Int {
        const idx = try self.add(Location{
            .code = .{
                .token = @intCast(self.locations.items.len),
                .temp = true
            },
            .typx = typx,
        });

        _ = try self.add(idx);
        return idx;
    }

    fn named(self: *Builder, name: []const u8, typx: Int) !Int {
        return self.add(Location{
            .code = .{
                .token = try self.add(name),
                .temp = false,
            },
            .typx = typx,
        });
    }

    fn flatten(self: *Builder, ctx: Context, tree: Ast, tokens: Tokens, table: Int, _block: Int, idx: Int) !State {
        const node = tree.at(idx);
        var block = _block;

        errdefer if (error_idx == null) { error_idx = idx ; };

        switch (node.kind) {
            .root => {
                const items = tree.extras(node.extra);

                for (items) |item| {
                    _, block = try self.flatten(ctx, tree, tokens, table, block, item);
                }

                return .{ 0, block };
            },
            .fdecl => {
                const name = tokens.slice(node.main+1);
                const blok = try self.newBlock();

                const bdx, block = try self.flatten(ctx, tree, tokens, idx, blok, node.extra.fdecl.body);
                _ = bdx;

                const symbol = try ctx.get(table, name);
                const proto = ctx.at(ATypx, symbol.typx).function;
                const p_names = ctx.slice(Int, proto.names, proto.len);
                const p_items = ctx.slice(Int, proto.items, proto.len);

                const ndx, const names = try self.newSlice([]const u8, proto.len);
                const pdx, const items = try self.newSlice(Typx, proto.len);

                for (p_names, names) |src, *dst|
                    dst.* = tokens.slice(src);

                for (p_items, items) |src, *dst|
                    dst.* = try self.trivialize(Typx, ctx, src);

                _ = try self.add(Function{
                    .ident = try self.add(name),
                    .proto = .{
                        .prms = .{
                            .names = ndx,
                            .items = pdx,
                            .len = proto.len,
                        },
                        .ret = try self.add(try self.trivialize(Typx, ctx, proto.ret)),
                    },
                    .varbs = try self.drive(),
                    .block = blok,
                });

                return .{ 0, block };
            },
            .fcall => {
                const call = node.extra.fcall;
                const fdx, block = try self.flatten(ctx, tree, tokens, table, block, call.func);

                const loc = self.at(Location, fdx);
                const dst = try self.auto(loc.typx);

                const list = tree.at(call.args);
                const items = tree.extras(list.extra);

                const len: u32 = @intCast(items.len);
                const adx, const args = try self.newSlice(Location, len);

                for (items, args) |src, *arg| {
                    const ldx, block = try self.flatten(ctx, tree, tokens, table, block, src);
                    arg.* = self.at(Location, ldx);
                }

                _ = try self.add(Inst{ .call = .{
                    .dst = dst,
                    .src = fdx,
                    .idx = adx,
                    .len = len,
                }});

                return .{ dst, block };
            },
            .integer => {
                const dst = try self.auto(try self.add(Typx{ .word = {} }));

                const text = tokens.slice(node.main);
                const int = try parseInt(i128, text, 0);

                const big = try Managed.initSet(self.allocator, int);
                const src = try self.add(Constant{ .primitive = big.toConst() });
                _ = try self.add(big);

                _ = try self.add(Inst{ .put = .{
                    .dst = dst,
                    .src = src,
                }});


                return .{ dst, block };
            },
            .identifier => {
                const name = tokens.slice(node.main);
                const symbol = try ctx.get(table, name);

                const typx = try self.trivialize(Typx, ctx, symbol.typx);
                const src = try self.named(name, try self.add(typx));

                return .{ src, block };
            },
            .vardef => {
                const name = tokens.slice(node.main+1);
                const symbol = try ctx.get(table, name);

                switch (try ctx.frameStorage(table)) {
                    .auto => {
                        const src, block = try self.flatten(ctx, tree, tokens, table, block, node.extra.bin_op.rhs);

                        const typx = try self.trivialize(Typx, ctx, symbol.typx);
                        const dst = try self.named(name, try self.add(typx));
                        _ = try self.add(dst);

                        _ = try self.add(Inst{ .mov = .{
                            .dst = dst,
                            .src = src,
                        }});

                        return .{ dst, block };
                    },
                    .root => {
                        //TODO(high), generate globals into executable
                        return .{ 0, block };
                    },
                }
            },
            .block => {
                const items = tree.extras(node.extra);

                for (items) |item| {
                    _, block = try self.flatten(ctx, tree, tokens, idx, block, item);
                }

                return .{ 0, block };
            },
            .add, .sub, .mul, .div => {
                const lhs, block = try self.flatten(ctx, tree, tokens, table, block, node.extra.bin_op.lhs);
                const rhs, block = try self.flatten(ctx, tree, tokens, table, block, node.extra.bin_op.rhs);

                const loc = self.at(Location, lhs);
                const dst = try self.auto(loc.typx);

                const op = Inst.BinOp{
                    .dst = dst,
                    .lhs = lhs,
                    .rhs = rhs,
                };

                const inst = switch (node.kind) {
                    .add => Inst{ .add = op },
                    .sub => Inst{ .sub = op },
                    .mul => Inst{ .mul = op },
                    .div => Inst{ .div = op },
                    else => unreachable,
                };

                _ = try self.add(inst);

                return .{ dst, block };
            },
            .ternary => {
                //TODO, figure out actual ternary type
                const dst = try self.auto(try self.add(Typx{ .word = {} }));

                const chs, const c_block = try self.flatten(ctx, tree, tokens, table, block, node.extra.tri_op.lhs);

                block = try self.newBlock();
                const lhs, const l_block = try self.flatten(ctx, tree, tokens, table, block, node.extra.tri_op.mhs+0);
                _ = try self.add(Inst{ .mov = .{
                    .dst = dst,
                    .src = lhs
                }});

                block = try self.newBlock();
                const rhs, const r_block = try self.flatten(ctx, tree, tokens, table, block, node.extra.tri_op.mhs+1);
                _ = try self.add(Inst{ .mov = .{
                    .dst = dst,
                    .src = rhs
                }});

                block = try self.newBlock();

                self.finishBlock(c_block, .{ .jnz = .{
                    .cond = chs,
                    .lhs = l_block,
                    .rhs = r_block,
                }});

                if (lhs != 0) self.finishBlock(l_block, .{ .jmp = block });
                if (rhs != 0) self.finishBlock(r_block, .{ .jmp = block });

                return .{ dst, block };
            },
            .ret => {
                const src, block = try self.flatten(ctx, tree, tokens, table, block, node.extra.mon_op);
                self.finishBlock(block, .{ .ret = src });

                return .{ 0, block };
            },
            else => std.debug.panic("TODO, handle flatten: {}", .{node.kind}),
        }
    }

    fn emit(self: Builder) !struct { Builder, Graph } {
        const graph = Graph{
            .functions = self.functions.items,
            .locations = self.locations.items,
            .constants = self.constants.items,
            .strings = self.strings.items,
            .blocks = self.blocks.items,
            .insts = self.insts.items,
            .typxs = self.typxs.items,
            .root = .{
                .varbs = .{
                    .items = 0,
                    .extra = 0,
                    .len = 0,
                },
            },
        };

        return .{ self, graph };
    }
};

pub fn flatten(gpa: Allocator, ctx: Context, tree: Ast, tokens: Tokens) !struct { Builder, Graph } {
    var builder = Builder {
        .allocator = gpa,
        .functions = .empty,
        .locations = .empty,
        .constants = .empty,
        .manageds = .empty,
        .strings = .empty,
        .blocks = .empty,
        .insts = .empty,
        .typxs = .empty,
        .local = .empty,
        .root = .{
            .varbs = .empty,
        },
    };

    const flow = try builder.flatten(ctx, tree, tokens, 0, 0, 0);
    _ = flow;

    return builder.emit();
}
