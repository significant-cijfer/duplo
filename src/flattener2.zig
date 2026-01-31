const std = @import("std");
const lego = @import("lego");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMapUnmanaged;

const Graph = lego.Graph;

const Lexer = @import("lexer.zig");
const Tokens = Lexer.Tokens;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

const Int = u32;

pub var error_idx: ?Int = null;

const Builder = struct {
    allocator: Allocator,
    functions: ArrayList(lego.Function),
    locations: ArrayList(lego.Location),
    constants: ArrayList(lego.Constant),
    strings: ArrayList([]const u8),
    blocks: ArrayList(lego.Block),
    insts: ArrayList(lego.Inst),
    typxs: ArrayList(lego.Typx),
    root: Root,

    const Root = struct {
        varbs: StringHashMap(Varb),
    };

    const Varb = struct {
        typx: lego.Int,
        con: lego.Int,
    };

    const Flow = struct {
        Int, // location
        Int, // block
    };

    fn deinit(self: Builder) void {
        self.functions.deinit(self.allocator);
        self.locations.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.strings.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.insts.deinit(self.allocator);
        self.typxs.deinit(self.allocator);

        self.root.varbs.deinit(self.allocator);
    }

    //fn flatten(self: *Graph, tables: *Tables, tree: Ast, tokens: Tokens, locals: Locals, tdx: u32, bdx: u32, idx: u32) !Flat {
    fn flatten(self: *Builder, tree: Ast, tokens: Tokens, bdx: Int, idx: Int) !Flow {
        _ = self;
        _ = tree;
        _ = tokens;
        _ = bdx;
        _ = idx;

        return error.TODO;
    }

    // deinit self here
    fn emit(self: Builder) !Graph {
        _ = self;

        return error.TODO;
    }
};

pub fn flatten(gpa: Allocator, tree: Ast, tokens: Tokens) !Graph {
    var builder = Builder {
        .allocator = gpa,
        .functions = .empty,
        .locations = .empty,
        .constants = .empty,
        .strings = .empty,
        .blocks = .empty,
        .insts = .empty,
        .typxs = .empty,
        .root = .{
            .varbs = .empty,
        },
    };

    const flow = try builder.flatten(tree, tokens, 0, 0);
    _ = flow;

    return builder.emit();
}
