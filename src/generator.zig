const std = @import("std");

const Writer = std.Io.Writer;

const Flattener = @import("flattener.zig");
const Graph = Flattener.Graph;

const backend_zig = @import("backend/zig.zig");

const Target = enum {
    zig,
};

pub fn generate(target: Target, writer: *Writer, graph: Graph) !void {
    return switch (target) {
        .zig => backend_zig.gen(writer, graph),
    };
}
