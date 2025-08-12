const std = @import("std");

pub const ArgumentType = enum(u8) {
    Int,
    Float,
    String,
    Bool,
};

fn ArgumentInfo(comptime EnumType: type) type {
    return struct {
        name: EnumType,
        value: ?[]const u8 = null,
        found: bool = false,
    };
}

// Public API: Zarg is a generic parser over an enum of flags/options.
pub fn Zarg(comptime EnumType: type) type {
    return struct {
        const Self = @This();
        const ArgInfo = ArgumentInfo(EnumType);

        allocator: std.mem.Allocator,
        arguments: std.ArrayList(ArgInfo),

        pub fn init(allocator: std.mem.Allocator) !Self {
            var self = Self{
                .allocator = allocator,
                .arguments = std.ArrayList(ArgInfo).init(allocator),
            };

            inline for (@typeInfo(EnumType).@"enum".fields) |field| {
                const enum_val = @field(EnumType, field.name);
                try self.arguments.append(ArgInfo{ .name = enum_val });
            }
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.arguments.deinit();
        }

        pub fn parse(self: *Self, args: [][:0]u8) !void {
            var i: usize = 1; // Skip program name
            while (i < args.len) : (i += 1) {
                const arg = args[i];
                if (!std.mem.startsWith(u8, arg, "--")) continue;
                const arg_name = arg[2..];

                for (self.arguments.items) |*arg_info| {
                    if (!std.mem.eql(u8, @tagName(arg_info.name), arg_name)) continue;
                    arg_info.found = true;

                    switch (arg_info.name.argType()) {
                        .Bool => arg_info.value = "true",
                        .Int, .Float, .String => {
                            if (i + 1 >= args.len) {
                                std.log.err("Missing value for argument --{s}", .{arg_name});
                                return error.MissingValue;
                            }
                            i += 1;
                            arg_info.value = args[i];
                        },
                    }
                    break;
                }
            }
        }

        pub fn printHelp(self: *Self) void {
            std.log.info("Available arguments:", .{});
            for (self.arguments.items) |arg_info| {
                const type_str = switch (arg_info.name.argType()) {
                    .Int => "integer",
                    .Float => "float",
                    .String => "string",
                    .Bool => "boolean flag",
                };
                std.log.info("  --{s} ({s})", .{ @tagName(arg_info.name), type_str });
            }
        }

        fn findArg(self: *Self, name: EnumType) ?*const ArgInfo {
            for (self.arguments.items) |*arg_info| {
                if (arg_info.name == name and arg_info.found) return arg_info;
            }
            return null;
        }

        pub fn getValue(self: *Self, comptime name: EnumType) ?switch (name.argType()) {
            .Int => i32,
            .Float => f32,
            .String => []const u8,
            .Bool => bool,
        } {
            return switch (comptime name.argType()) {
                .Int => if (self.findArg(name)) |arg| if (arg.value) |v| std.fmt.parseInt(i32, v, 10) catch null else null else null,
                .Float => if (self.findArg(name)) |arg| if (arg.value) |v| std.fmt.parseFloat(f32, v) catch null else null else null,
                .String => if (self.findArg(name)) |arg| arg.value else null,
                .Bool => self.findArg(name) != null,
            };
        }
    };
}

// ------------------ Tests ------------------

test "parse flags and values" {
    const allocator = std.testing.allocator;

    const Args = enum(u8) {
        age,
        gpa,
        name,
        is_student,

        pub fn argType(self: @This()) ArgumentType {
            return switch (self) {
                .age => .Int,
                .gpa => .Float,
                .name => .String,
                .is_student => .Bool,
            };
        }
    };

    var z = try Zarg(Args).init(allocator);
    defer z.deinit();

    var argv = std.ArrayList([:0]u8).init(allocator);
    defer argv.deinit();

    // helper to make sentinel-terminated strings
    const mk = struct {
        fn c(s: []const u8) ![:0]u8 {
            return try std.mem.concatWithSentinel(allocator, u8, &.{ s }, 0);
        }
    };

    defer {
        // free all created strings
        for (argv.items) |s| allocator.free(s);
    }

    try argv.append(try mk.c("zarg"));
    try argv.append(try mk.c("--age"));
    try argv.append(try mk.c("30"));
    try argv.append(try mk.c("--gpa"));
    try argv.append(try mk.c("3.8"));
    try argv.append(try mk.c("--name"));
    try argv.append(try mk.c("Alice"));
    try argv.append(try mk.c("--is_student"));

    try z.parse(argv.items);

    try std.testing.expectEqual(@as(?i32, 30), z.getValue(.age));
    try std.testing.expectEqual(@as(?f32, 3.8), z.getValue(.gpa));
    try std.testing.expectEqualStrings("Alice", z.getValue(.name).?);
    try std.testing.expectEqual(true, z.getValue(.is_student));
}
