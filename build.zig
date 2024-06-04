const std = @import("std");

const test_paths = [_][]const u8{
    "plus",
    "question",
    "star",
    "tuple",
    "acceptor",
    "cut",
    "star_cut",
    "new_calculator",
    "json",
};

const Version = std.SemanticVersion{
    .major = 0,
    .minor = 1,
    .patch = 0,
    .pre = null,
    .build = null,
};

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "zapp",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const options = b.addOptions();

    options.addOption(std.SemanticVersion, "Version", Version);
    exe.root_module.addOptions("config", options);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // test
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    for (test_paths) |path| {
        const gen_cmd = b.addRunArtifact(exe);
        const grammar_path = b.fmt("test/{s}/grammar.peg", .{path});
        const main_path = b.fmt("test/{s}/main.zig", .{path});
        const generated_file = b.fmt("test/{s}/{s}.zig", .{ path, path });

        gen_cmd.addArg("gen");
        gen_cmd.addFileArg(b.path(grammar_path));
        const generated = gen_cmd.addOutputFileArg(generated_file);
        // install generated files to zig-out/gen so that newest versions are
        // always available there
        const install_gen = b.addInstallFileWithDir(
            generated,
            .{ .custom = "gen" },
            b.fmt("{s}.zig", .{path}),
        );

        const grammar_unit_tests = b.addTest(.{
            .root_source_file = b.path(main_path),
            .target = target,
            .optimize = optimize,
        });

        grammar_unit_tests.root_module.addAnonymousImport("parser", .{
            .root_source_file = generated,
        });

        const run_grammar_unit_tests = b.addRunArtifact(grammar_unit_tests);
        test_step.dependOn(&run_grammar_unit_tests.step);
        test_step.dependOn(&install_gen.step);
    }

    // bench
    const bench_step = b.step("bench", "benchmark a grammar");
    const bench_grammar = b.option(
        []const u8,
        "bench_grammar",
        "the input grammar (only for bench), default: zig.peg",
    ) orelse "./grammars/zig.peg";
    const bench_dir = b.option(
        []const u8,
        "bench_dir",
        "the directory to run the benchmark on, default: src",
    ) orelse "./src";
    const show_memo_info = b.option(
        bool,
        "memo_info",
        "show the rule that reset the largest memo for each file",
    ) orelse false;

    const gen_cmd = b.addRunArtifact(exe);
    gen_cmd.addArg("gen");
    gen_cmd.addFileArg(b.path(bench_grammar));
    const generated = gen_cmd.addOutputFileArg("out.zig");
    gen_cmd.addArg("--name=Parser");

    const bench = b.addExecutable(.{
        .name = "bench",
        .root_source_file = b.path("src/bench_main.zig"),
        .target = target,
        .optimize = optimize,
        .omit_frame_pointer = false,
    });
    bench.root_module.addAnonymousImport("parser", .{
        .root_source_file = generated,
    });
    const bench_run = b.addRunArtifact(bench);
    bench_run.addArgs(&.{ bench_dir, if (show_memo_info) "t" else "f" });
    bench_step.dependOn(&bench_run.step);
}
