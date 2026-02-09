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
const StringList = lego.StringList;
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
        imports: ArrayList(Import),
        externs: ArrayList(Extern),
        varbs: ArrayList(Varb),
    };

    const Import = struct {
        name: Int,
        typx: Int,
    };

    const Extern = struct {
        local: Location,
    };

    const Varb = struct {
        name: Int,
        local: Int,
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

        self.root.imports.deinit(self.allocator);
        self.root.externs.deinit(self.allocator);
        self.root.varbs.deinit(self.allocator);
    }

    fn listOf(self: Builder, comptime T: type) []const T {
        return switch (T) {
            Function => self.functions.items,
            Location => self.locations.items,
            Constant => self.constants.items,
            Managed => self.manageds.items,
            []const u8 => self.strings.items,
            Block => self.blocks.items,
            Inst => self.insts.items,
            Typx => self.typxs.items,
            Int => self.local.items,
            Import => self.root.imports.items,
            Extern => self.root.externs.items,
            Varb => self.root.varbs.items,
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
            Import => &self.root.imports,
            Extern => &self.root.externs,
            Varb => &self.root.varbs,
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

    //TODO(high), figure out a way to not have to use names when trivializing functions
    fn trivialize(self: *Builder, ctx: Context, tokens: Tokens, typx: Int) !Typx {
        return switch (ctx.at(ATypx, typx)) {
            .int => |i| .{ .primitive = .{
                .bits = i.bits,
                .sign = i.sign,
            }},
            .ct_int => .{ .word = {} },
            .function => |f| {
                const p_items = ctx.slice(Int, f.items, f.len);
                const pdx, const items = try self.newSlice(Typx, f.len);

                for (p_items, items) |src, *dst|
                    dst.* = try self.trivialize(ctx, tokens, src);

                return .{ .function = .{
                    .prms = pdx,
                    .len = f.len,
                    .ret = try self.add(try self.trivialize(ctx, tokens, f.ret)),
                }};
            },
            else => |t| std.debug.panic("TODO, handle trivialize for: {s}", .{ @tagName(t) }),
        };
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
            .edecl => {
                const item = tree.at(node.extra.mon_op);

                switch (item.kind) {
                    .fproto => {
                        const name = tokens.slice(node.main+2);
                        const symbol = try ctx.get(table, name);

                        _ = try self.add(Extern{
                            .local = .{
                                .code = .{
                                    .token = try self.add(name),
                                    .temp = false,
                                },
                                .typx = try self.add(try self.trivialize(ctx, tokens, symbol.typx)),
                            },
                        });
                    },
                    else => |k| std.debug.panic("TODO: handle edecl: {}", .{k}),
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
                const names = ctx.slice(Int, proto.names, proto.len);
                const items = ctx.slice(Int, proto.items, proto.len);

                const ldx, const prms = try self.newSlice(Location, proto.len);

                for (names, items, prms) |src, item, *dst| {
                    dst.* = .{
                        .code = .{
                            .token = try self.add(tokens.slice(src)),
                            .temp = false,
                        },
                        .typx = try self.add(try self.trivialize(ctx, tokens, item)),
                    };
                }

                _ = try self.add(Function{
                    .ident = try self.add(name),
                    .proto = .{
                        .prms = .{
                            .items = ldx,
                            .len = proto.len,
                        },
                        .ret = try self.add(try self.trivialize(ctx, tokens, proto.ret)),
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
                const fun = self.at(Typx, loc.typx).function;
                const dst = try self.auto(fun.ret);

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

                const typx = try self.trivialize(ctx, tokens, symbol.typx);
                const src = try self.named(name, try self.add(typx));

                return .{ src, block };
            },
            .vardef => {
                const name = tokens.slice(node.main+1);
                const symbol = try ctx.get(table, name);

                switch (try ctx.frameStorage(table)) {
                    .auto => {
                        const src, block = try self.flatten(ctx, tree, tokens, table, block, node.extra.bin_op.rhs);

                        const typx = try self.trivialize(ctx, tokens, symbol.typx);
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

    fn emit(self: *Builder) !struct { Builder, Graph } {
        var names = ArrayList([]const u8).empty;
        var typxs = ArrayList(Typx).empty;
        var e_locs = ArrayList(Location).empty;
        var v_locs = ArrayList(Location).empty;
        var cons = ArrayList(Constant).empty;

        defer names.deinit(self.allocator);
        defer typxs.deinit(self.allocator);
        defer e_locs.deinit(self.allocator);
        defer v_locs.deinit(self.allocator);
        defer cons.deinit(self.allocator);

        for (self.root.imports.items) |import| {
            try names.append(self.allocator, self.at([]const u8, import.name));
            try typxs.append(self.allocator, self.at(Typx, import.typx));
        }

        for (self.root.externs.items) |ext| {
            try e_locs.append(self.allocator, ext.local);
        }

        for (self.root.varbs.items) |varb| {
            try v_locs.append(self.allocator, self.at(Location, varb.local));
        }

        const imports = StringList{
            .names = try self.addSlice(names.items),
            .items = try self.addSlice(typxs.items),
            .len = @intCast(self.root.imports.items.len),
        };

        const externs = LocationList{
            .items = try self.addSlice(e_locs.items),
            .len = @intCast(self.root.externs.items.len),
        };

        const varbs = LocationExtraList{
            .items = try self.addSlice(v_locs.items),
            .extra = try self.addSlice(cons.items),
            .len = @intCast(self.root.varbs.items.len),
        };

        const graph = Graph{
            .functions = self.functions.items,
            .locations = self.locations.items,
            .constants = self.constants.items,
            .strings = self.strings.items,
            .blocks = self.blocks.items,
            .insts = self.insts.items,
            .typxs = self.typxs.items,
            .root = .{
                .imports = imports,
                .externs = externs,
                .varbs = varbs,
            },
        };

        return .{ self.*, graph };
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
            .imports = .empty,
            .externs = .empty,
            .varbs = .empty,
        },
    };

    const flow = try builder.flatten(ctx, tree, tokens, 0, 0, 0);
    _ = flow;

    return builder.emit();
}
