const std = @import("std");

// Vtable entry for subcommands (opaque pointer + parse thunk)
const Subcommand = struct {
    ptr: *anyopaque,
    parse: *const fn (*anyopaque, [][:0]u8) anyerror!void,
};

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
        subcommands: std.StringHashMap(Subcommand),
        active_subcommand: ?*anyopaque = null,
        active_sub_name: ?[]const u8 = null,

        pub fn init(allocator: std.mem.Allocator) !Self {
            var self = Self{
                .allocator = allocator,
                .arguments = std.ArrayList(ArgInfo).init(allocator),
                .subcommands = std.StringHashMap(Subcommand).init(allocator),
            };

            inline for (@typeInfo(EnumType).@"enum".fields) |field| {
                const enum_val = @field(EnumType, field.name);
                try self.arguments.append(ArgInfo{ .name = enum_val });
            }
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.arguments.deinit();

            // Subcommand instances are owned by the caller/tests; nothing to deinit here.
            var it = self.subcommands.iterator();
            while (it.next()) |_| {}
            self.subcommands.deinit();
        }

        // Register a subcommand parser of arbitrary type `T` that exposes `parse`.
        fn _addSubcommand(self: *Self, comptime T: type, name: []const u8, parser: *T) !void {
            const Thunk = struct {
                fn call(p: *anyopaque, a: [][:0]u8) !void {
                    const sp = @as(*T, @ptrCast(@alignCast(p)));
                    try T.parse(sp, a);
                }
            };
            try self.subcommands.put(name, .{ .ptr = parser, .parse = Thunk.call });
        }

        // Convenience: infer type from the parser pointer argument
        pub fn addSubcommand(self: *Self, name: []const u8, parser: anytype) !void {
            const T = @TypeOf(parser.*);
            try self._addSubcommand(T, name, parser);
        }

        pub fn activeSubcommandName(self: *Self) ?[]const u8 {
            return self.active_sub_name;
        }

        pub fn getSubcommandPtr(self: *Self, name: []const u8) ?*anyopaque {
            if (self.subcommands.get(name)) |sc| return sc.ptr;
            return null;
        }

        pub fn activeIs(self: *Self, name: []const u8) bool {
            if (self.active_sub_name) |n| return std.mem.eql(u8, n, name);
            return false;
        }

        pub fn withSub(self: *Self, comptime name: []const u8, comptime T: type, handler: *const fn (*T, std.mem.Allocator) anyerror!void) !void {
            if (self.subcommands.get(name)) |sc| {
                const sp = @as(*T, @ptrCast(@alignCast(sc.ptr)));
                try handler(sp, self.allocator);
            } else return error.UnknownSubcommand;
        }

        // Return the typed subparser if it's the active one; otherwise null
        pub fn sub(self: *Self, comptime name: []const u8, comptime T: type) ?*T {
            if (!self.activeIs(name)) return null;
            if (self.subcommands.get(name)) |sc| {
                return @as(*T, @ptrCast(@alignCast(sc.ptr)));
            }
            return null;
        }

        // Conditionally run a handler for a typed subcommand; returns whether it handled.
        // Accepts a handler holder type with a static function: `pub fn handler(*T) !void`.
        pub fn on(self: *Self, comptime name: []const u8, comptime T: type, comptime Handler: type) !bool {
            if (!self.activeIs(name)) return false;
            try self.withSub(name, T, Handler.handler);
            return true;
        }

        pub fn parse(self: *Self, args: [][:0]u8) !void {
            var i: usize = 0; // Start from 0; skip non-flags dynamically

            // Check for subcommands first (only when the second arg is a non-flag token)
            if (args.len > 1) {
                const maybe_sub = args[1];
                if (!std.mem.startsWith(u8, maybe_sub, "--")) {
                    if (self.subcommands.get(maybe_sub)) |sc| {
                        self.active_subcommand = sc.ptr;
                        self.active_sub_name = maybe_sub;
                        try sc.parse(sc.ptr, args[2..]); // Delegate remaining args to sub-parser
                        return;
                    }
                }
            }

            // If no subcommand is found, parse arguments (skip any non-flag tokens)
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
            const indent = "  ";

            if (self.active_sub_name) |name| {
                std.log.info("Subcommand: {s}", .{name});
                std.log.info("Available arguments:", .{});
            } else {
                std.log.info("Available arguments:", .{});
                if (self.subcommands.count() > 0) {
                    std.log.info("Available subcommands:", .{});
                    var it = self.subcommands.iterator();
                    while (it.next()) |entry| {
                        std.log.info("{s}", .{entry.key});
                    }
                    std.log.info("", .{}); // Blank line for spacing
                }
            }

            for (self.arguments.items) |arg_info| {
                const type_str = switch (arg_info.name.argType()) {
                    .Int => "integer",
                    .Float => "float",
                    .String => "string",
                    .Bool => "boolean flag",
                };
                std.log.info("{s}--{s} ({s})", .{ indent, @tagName(arg_info.name), type_str });
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

    const mk = struct {
        fn c(s: []const u8) ![:0]u8 {
            return try std.mem.concatWithSentinel(allocator, u8, &.{s}, 0);
        }
    };

    defer {
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

test "parse subcommands" {
    const allocator = std.testing.allocator;

    const MainArgs = enum {
        help,
        pub fn argType(self: @This()) ArgumentType {
            return switch (self) {
                .help => .Bool,
            };
        }
    };

    const EncodeArgs = enum {
        age,
        name,
        pub fn argType(self: @This()) ArgumentType {
            return switch (self) {
                .age => .Int,
                .name => .String,
            };
        }
    };

    const DecodeArgs = enum {
        school,
        year,
        pub fn argType(self: @This()) ArgumentType {
            return switch (self) {
                .school => .String,
                .year => .Int,
            };
        }
    };

    var z = try Zarg(MainArgs).init(allocator);
    defer z.deinit();

    var encode_parser = try Zarg(EncodeArgs).init(allocator);
    defer encode_parser.deinit();
    var decode_parser = try Zarg(DecodeArgs).init(allocator);
    defer decode_parser.deinit();

    try z.addSubcommand("encode", &encode_parser);
    try z.addSubcommand("decode", &decode_parser);

    var argv = std.ArrayList([:0]u8).init(allocator);
    defer argv.deinit();

    const mk = struct {
        fn c(s: []const u8) ![:0]u8 {
            return try std.mem.concatWithSentinel(allocator, u8, &.{s}, 0);
        }
    };
    defer {
        for (argv.items) |s| allocator.free(s);
    }

    try argv.append(try mk.c("program"));
    try argv.append(try mk.c("encode"));
    try argv.append(try mk.c("--age"));
    try argv.append(try mk.c("12"));
    try argv.append(try mk.c("--name"));
    try argv.append(try mk.c("burak"));

    try z.parse(argv.items);

    try std.testing.expect(try z.on("encode", Zarg(EncodeArgs), struct {
        fn handler(p: *Zarg(EncodeArgs), _: std.mem.Allocator) !void {
            try std.testing.expectEqual(@as(?i32, 12), p.getValue(.age));
            try std.testing.expectEqualStrings("burak", p.getValue(.name).?);
        }
    }));

    try std.testing.expect(!(try z.on("decode", Zarg(DecodeArgs), struct {
        fn handler(p: *Zarg(DecodeArgs)) !void {
            try std.testing.expectEqualStrings("MIT", p.getValue(.school).?);
            try std.testing.expectEqual(@as(?i32, 2023), p.getValue(.year));
        }
    })));
}
