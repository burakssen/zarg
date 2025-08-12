const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    // Expose library module for consumers: b.dependency("zarg", ...) in other projects
    const zarg_mod = b.addModule("zarg", .{
        .root_source_file = b.path("src/zarg.zig"),
        .target = target,
        .optimize = optimize,
    });

    _ = zarg_mod; // currently unused within this build, but exported for dependents

    // Unit tests for the library
    const lib_tests = b.addTest(.{
        .root_source_file = b.path("src/zarg.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_tests = b.addRunArtifact(lib_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}
