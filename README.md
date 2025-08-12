# Zarg — A Generic Command-Line Argument Parser for Zig

`Zarg` is a small, type-safe command-line argument parser for Zig.  
It uses an enum to define available arguments, automatically inferring their types and parsing them from `argv`.

## Features

- **Enum-driven API** — Declare arguments in an enum and specify their type with a `argType()` method.
- **Automatic type parsing** — Supports `Int`, `Float`, `String`, and `Bool` flags.
- **Generates help text** — Prints available arguments with their expected types.
- **Type-safe retrieval** — `getValue()` returns the correct type for each argument.

---

## Installation

Copy `Zarg` into your Zig project as a `.zig` file and `@import` it:

```zig
const std = @import("std");
const Zarg = @import("zarg.zig").Zarg;

const ArgumentType = enum(u8) { Int, Float, String, Bool };

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
```

### Usage Example

```zig
const std = @import("std");
const Zarg = @import("zarg.zig").Zarg;
const allocator = std.heap.page_allocator;

pub fn main() !void {
    var z = try Zarg(Args).init(allocator);
    defer z.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    try z.parse(args);

    if (z.getValue(.age)) |age| {
        std.log.info("Age: {d}", .{age});
    }
    if (z.getValue(.gpa)) |gpa| {
        std.log.info("GPA: {d:.2}", .{gpa});
    }
    if (z.getValue(.name)) |name| {
        std.log.info("Name: {s}", .{name});
    }
    if (z.getValue(.is_student)) {
        std.log.info("User is a student", .{});
    }
}
```

### Example Command

```bash
$ ./myprogram --age 30 --gpa 3.8 --name Alice --is_student
Age: 30
GPA: 3.80
Name: Alice
User is a student
```

# Api

`Zarg(EnumType)`
Creates a parser for the given enum type.

Methods:

- `init(allocator: std.mem.Allocator) !Self`
  - Initializes the parser, populating available arguments from the enum.
- `deinit()`
  - Frees internal resources.
- `parse(args: [][:0]u8) !void`
  - Parses command-line arguments.
- `printHelp()`
  - Prints help text.
- `getValue(comptime name: EnumType) ?T`
  - Retrieves the parsed value for the argument, returning null if missing.
  - The type T is automatically determined from the enum's argType().

# Error Handling

- `error.MissingValue` -- Raised when an argument that requires a value is given without one.
