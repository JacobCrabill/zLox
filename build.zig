const std = @import("std");
const builtin = @import("builtin");

/// Ensure a compatible version of the Zig compiler
pub fn checkZigVersion() void {
    const current_zig = builtin.zig_version;
    const min_zig = std.SemanticVersion.parse("0.12.0-dev.2030") catch unreachable; // build system changes: ziglang/zig#18160
    if (current_zig.order(min_zig) == .lt) {
        @compileError(std.fmt.comptimePrint("Your Zig version v{} does not meet the minimum build requirement of v{}", .{
            current_zig,
            min_zig,
        }));
    }
}

pub fn build(b: *std.Build) void {
    comptime {
        checkZigVersion();
    }

    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const optimize = b.standardOptimizeOption(.{});

    // Compile the main executable
    const exe = b.addExecutable(.{
        .name = "zlox",
        .root_source_file = .{ .path = "src/main.zig" },
        .version = .{ .major = 0, .minor = 1, .patch = 0 },
        .optimize = optimize,
        .target = target,
    });

    b.installArtifact(exe);

    const app_step = b.step("app", "Build the app ('zigdown' executable)");
    app_step.dependOn(&exe.step);

    // Configure how the main executable should be run
    const app = b.addRunArtifact(exe);
    app.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        app.addArgs(args);
    }

    // Add a run step to run the executable
    const run_step = b.step("run", "Run the app (use `-- <args>` to supply arguments)");
    run_step.dependOn(&app.step);

    // Add unit tests
    // addTest(b, "test-lexer", "Run Lexer unit tests", "src/lexer.zig", optimize);
    // addTest(b, "test-parser", "Run parser unit tests", "src/parser.zig", optimize);
    // addTest(b, "test-render", "Run renderer unit tests", "src/render.zig", optimize);
    // addTest(b, "test-image", "Run the image rendering tests", "src/image.zig", optimize);

    // addTest(b, "test-all", "Run all unit tests", "src/test.zig", optimize);
}

/// Add a unit test step using the given file
///
/// @param[inout] b: Mutable pointer to the Build object
/// @param[in] cmd: The build step name ('zig build cmd')
/// @param[in] description: The description for 'zig build -l'
/// @param[in] path: The zig file to test
/// @param[in] optimize: Build optimization settings
fn addTest(b: *std.Build, cmd: []const u8, description: []const u8, path: []const u8, optimize: std.builtin.Mode) void {
    const test_exe = b.addTest(.{
        .root_source_file = .{ .path = path },
        .optimize = optimize,
    });
    const run_step = b.addRunArtifact(test_exe);
    run_step.has_side_effects = true; // Force the test to always be run on command
    const step = b.step(cmd, description);
    step.dependOn(&run_step.step);
}
