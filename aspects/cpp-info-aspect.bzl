# Bazel aspect for extracting C++ compile information.
# Produces per-target .cpp_info.json files in bazel-bin/ containing
# compiler path, flags, and source file list — used to assemble
# compile_commands.json for clangd.
#
# Usage:
#   bazel build //pkg/... \
#     --aspects=.bazel-compdb/cpp-info-aspect.bzl%cpp_info_aspect \
#     --output_groups=cpp_info_files \
#     --keep_going
#
# Reference: https://bazel.build/extending/aspects
#
# Copyright 2026 lollinus
# SPDX-License-Identifier: Apache-2.0

load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "CPP_COMPILE_ACTION_NAME",
    "C_COMPILE_ACTION_NAME",
)
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")

CppInfoAspectInfo = provider(
    doc = "Marker provider for cpp_info_aspect propagation.",
    fields = {},
)

def _collect_sources(target, ctx):
    """Collect C/C++ source and header files from rule attributes and providers."""
    srcs = []

    # Explicit srcs attribute
    if hasattr(ctx.rule.attr, "srcs"):
        for src in ctx.rule.attr.srcs:
            if hasattr(src, "files"):
                for f in src.files.to_list():
                    if f.extension in ("c", "cc", "cpp", "cxx", "h", "hh", "hpp", "hxx"):
                        srcs.append(f)

    # Explicit hdrs attribute
    if hasattr(ctx.rule.attr, "hdrs"):
        for hdr in ctx.rule.attr.hdrs:
            if hasattr(hdr, "files"):
                for f in hdr.files.to_list():
                    if f.extension in ("h", "hh", "hpp", "hxx"):
                        srcs.append(f)

    # Generated files (e.g. from cc_proto_library)
    for f in target.files.to_list():
        if f.extension in ("c", "cc", "cpp", "cxx", "h", "hh", "hpp", "hxx"):
            if f not in srcs:
                srcs.append(f)

    # Fallback: CcInfo direct headers
    if not srcs and CcInfo in target:
        for hdr in target[CcInfo].compilation_context.direct_headers:
            srcs.append(hdr)

    return srcs

def _extract_compile_info(ctx, target, cc_toolchain, feature_configuration):
    """Extract compiler path and flags from the C++ toolchain and target."""
    compiler = str(cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
    ))

    compile_variables = cc_common.create_compile_variables(
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        user_compile_flags = ctx.fragments.cpp.cxxopts + ctx.fragments.cpp.copts,
        add_legacy_cxx_options = True,
    )

    flags = list(cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = CPP_COMPILE_ACTION_NAME,
        variables = compile_variables,
    ))

    # Per-target copts
    if hasattr(ctx.rule.attr, "copts"):
        flags.extend(ctx.rule.attr.copts)

    # Includes, defines, system includes from CcInfo
    compilation_context = target[CcInfo].compilation_context

    for define in compilation_context.defines.to_list():
        flags.append("-D" + define)

    for define in compilation_context.local_defines.to_list():
        flags.append("-D" + define)

    for inc in compilation_context.includes.to_list():
        flags.append("-I" + inc)

    for inc in compilation_context.external_includes.to_list():
        flags.extend(["-isystem", inc])

    for inc in compilation_context.system_includes.to_list():
        flags.extend(["-isystem", inc])

    for inc in compilation_context.quote_includes.to_list():
        flags.extend(["-iquote", inc])

    return struct(
        compiler = compiler,
        args = flags,
    )

def _cpp_info_aspect_impl(target, ctx):
    """Aspect implementation: emit .cpp_info.json for each cc_* target."""

    # Only process targets that have CcInfo and are cc_* rules
    if not CcInfo in target or ctx.rule.kind.find("cc") == -1:
        # Propagate through deps even for non-cc targets
        transitive = []
        for attr_name in ("deps", "implementation_deps"):
            if hasattr(ctx.rule.attr, attr_name):
                for dep in getattr(ctx.rule.attr, attr_name):
                    if OutputGroupInfo in dep and hasattr(dep[OutputGroupInfo], "cpp_info_files"):
                        transitive.append(dep[OutputGroupInfo].cpp_info_files)

        return [
            CppInfoAspectInfo(),
            OutputGroupInfo(
                cpp_info_files = depset([], transitive = transitive),
            ),
        ]

    # Set up C++ toolchain and feature configuration
    cc_toolchain = find_cpp_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features + ["dependency_file"],
    )

    srcs = _collect_sources(target, ctx)

    # Extract compile information
    compile_info = _extract_compile_info(ctx, target, cc_toolchain, feature_configuration)

    # Build the JSON content
    cpp_info = struct(
        label = str(ctx.label),
        compiler = compile_info.compiler,
        args = compile_info.args,
        files = [src.path for src in srcs],
    )

    # Write per-target JSON file
    output = ctx.actions.declare_file(ctx.label.name + ".cpp_info.json")
    ctx.actions.write(
        content = cpp_info.to_json(),
        output = output,
    )

    # Collect transitive cpp_info_files from deps
    transitive = []
    for attr_name in ("deps", "implementation_deps"):
        if hasattr(ctx.rule.attr, attr_name):
            for dep in getattr(ctx.rule.attr, attr_name):
                if OutputGroupInfo in dep and hasattr(dep[OutputGroupInfo], "cpp_info_files"):
                    transitive.append(dep[OutputGroupInfo].cpp_info_files)

    return [
        CppInfoAspectInfo(),
        OutputGroupInfo(
            cpp_info_files = depset([output], transitive = transitive),
        ),
    ]

cpp_info_aspect = aspect(
    implementation = _cpp_info_aspect_impl,
    attr_aspects = ["deps", "implementation_deps"],
    attrs = {
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    fragments = ["cpp"],
    required_aspect_providers = [CppInfoAspectInfo],
    toolchains = ["@bazel_tools//tools/cpp:toolchain_type"],
)
