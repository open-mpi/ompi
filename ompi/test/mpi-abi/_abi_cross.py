#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""MPICH cross-implementation ``check-abi-mpich`` discovery,
environment reporting, and cross-direction probe execution."""

import os
import platform
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    MPI_REMOVED_LEGACY_C_NAMES, SKIP_CROSS_PROBES_NOT_PASSED,
    SKIP_FORTRAN_BINDINGS_DISABLED, _append_check, _check_counts,
    _env_positive_int, _extend_checks, _fail, _pass, _skip, _write_text, _xfail)
from _abi_tables import (
    INSTALLED_C_ABI_PROBES, INSTALLED_C_CALLBACK_PROBES,
    INSTALLED_C_RUNTIME_API_PROBES)
from _abi_discovery import (
    _discover_mpi_installations, _open_mpi_mpifort, _select_install,
    _tool_available, _tool_override_used)
from _abi_probes import (
    _callback_api_coverage_audit, _fortran_bindings_enabled,
    _optional_feature_info, _optional_feature_skip_reason,
    _parse_header_constant_names, _prepare_installed_c_probe_body,
    _runtime_api_probe_generation_check, _runtime_probe_constant_names,
    _runtime_probe_generation_check)
from _abi_installed import (
    _c_probe_source, _command_result,
    _cross_compile_extra_flags, _cross_compile_header,
    _cross_compile_overrides, _cross_header_semantic_checks,
    _cross_launcher_args, _cross_linkage_diagnostic_check,
    _cross_runtime_loader_policy, _cross_test_dirs,
    _cross_verify_or_rewrite_abi_libraries, _cross_wrapper_intent_check,
    _mpich_transport_runtime_env, _runtime_env_for_implementation)


def _tool_info(mode):
    """Resolve tool paths, rank counts, and optional override paths."""
    runtime_loader = _runtime_loader_var()
    discovery = _discover_mpi_installations(
        include_open_mpi=mode in ("check-abi", "check-abi-mpich"),
        include_mpich=mode == "check-abi-mpich")
    # Normalize the selected installs to empty dicts so the record-building
    # below uses uniform .get() access instead of repeating an
    # "... if install is not None else <default>" guard for every field.
    open_mpi_install = _select_install(discovery, "open_mpi") or {}
    mpich_install = _select_install(discovery, "mpich") or {}
    cross_directions = _cross_direction_selection()
    open_mpi_tools = open_mpi_install.get("tools", {})
    mpich_tools = mpich_install.get("tools", {})
    open_mpi_evidence = open_mpi_install.get("evidence", {})
    mpich_evidence = mpich_install.get("evidence", {})
    open_mpi_found_paths = open_mpi_install.get("paths", {})
    mpich_found_paths = mpich_install.get("paths", {})
    open_mpi_include_dirs = open_mpi_found_paths.get("include_dirs", [])
    open_mpi_library_dirs = open_mpi_found_paths.get("library_dirs", [])
    mpich_include_dirs = mpich_found_paths.get("include_dirs", [])
    mpich_library_dirs = mpich_found_paths.get("library_dirs", [])
    mpich_device = mpich_evidence.get("mpich_device")
    open_mpi_paths = {
        "include": (
            os.environ.get("OMPI_ABI_TEST_INCLUDE_PATH") or
            (open_mpi_include_dirs[0] if open_mpi_include_dirs else None)
        ),
        "library": (
            os.environ.get("OMPI_ABI_TEST_LIBRARY_PATH") or
            (open_mpi_library_dirs[0] if open_mpi_library_dirs else None)
        ),
        "launcher_args": os.environ.get("OMPI_ABI_TEST_LAUNCHER_ARGS"),
        "runtime_loader": runtime_loader,
    }
    mpich_paths = {
        "include": (
            os.environ.get("MPICH_ABI_TEST_INCLUDE_PATH") or
            (mpich_include_dirs[0] if mpich_include_dirs else None)
        ),
        "library": (
            os.environ.get("MPICH_ABI_TEST_LIBRARY_PATH") or
            (mpich_library_dirs[0] if mpich_library_dirs else None)
        ),
        "launcher_args": os.environ.get("MPICH_ABI_TEST_LAUNCHER_ARGS"),
        "runtime_loader": runtime_loader,
    }
    tools = {
        "open_mpi": {
            "mpicc_abi": open_mpi_tools.get("mpicc_abi"),
            "mpifort": _open_mpi_mpifort(open_mpi_install),
            "mpirun": open_mpi_tools.get("mpirun"),
            "identity": open_mpi_install.get("implementation"),
            "mpi_forum_abi_available": open_mpi_evidence.get(
                "mpi_forum_abi_available"),
            "unsuitable_reasons": open_mpi_evidence.get("unsuitable_reasons"),
            "prefix": open_mpi_install.get("prefix"),
        },
        "mpich": {
            "mpicc": mpich_tools.get("mpicc"),
            "mpirun": mpich_tools.get("mpirun"),
            "identity": mpich_install.get("implementation"),
            "mpi_forum_abi_available": mpich_evidence.get(
                "mpi_forum_abi_available"),
            "device": mpich_device,
            "unsuitable_reasons": mpich_evidence.get("unsuitable_reasons"),
            "prefix": mpich_install.get("prefix"),
        },
        "rank_counts": {
            "np1": _env_positive_int("OMPI_ABI_TEST_NP1", 1),
            "np2": _env_positive_int("OMPI_ABI_TEST_NP2", 2),
        },
        "paths": open_mpi_paths,
        "discovery": discovery,
    }
    if mode == "check-abi-mpich":
        tools["cross"] = {
            "platform": platform.system(),
            "runtime_loader": runtime_loader,
            "directions": cross_directions["directions"],
            "direction_error": cross_directions["error"],
            "direction_invalid_values": cross_directions["invalid_values"],
            "direction_raw_value": cross_directions["raw_value"],
            "open_mpi_paths": open_mpi_paths,
            "mpich_paths": mpich_paths,
            "mpich_transport_env": _mpich_transport_runtime_env(
                mpich_device),
        }
    if mode != "check-abi-mpich":
        tools.pop("mpich")
    return tools


def _cross_direction_selection(raw=None):
    """Return requested cross-test directions and invalid input details.

    12A records direction selection before 12B starts executing probes.
    The default is both standard ABI directions.  The environment knob is
    intentionally simple, but typos must be hard failures because a CI
    mistake should not produce a passing run with zero tested directions.
    """
    if raw is None:
        raw = os.environ.get("OMPI_ABI_TEST_MPICH_DIRECTIONS", "both")
    values = [item.strip() for item in raw.split(",") if item.strip()]
    aliases = {
        "both": "both",
        "mpich-to-ompi": "mpich_compile_open_mpi_run",
        "mpich_compile_open_mpi_run": "mpich_compile_open_mpi_run",
        "ompi-to-mpich": "open_mpi_compile_mpich_run",
        "open_mpi_compile_mpich_run": "open_mpi_compile_mpich_run",
    }
    directions = []
    invalid_values = []
    if not values:
        invalid_values.append("<empty>")
    for value in values:
        direction = aliases.get(value)
        if direction == "both":
            for default_direction in (
                    "mpich_compile_open_mpi_run",
                    "open_mpi_compile_mpich_run"):
                if default_direction not in directions:
                    directions.append(default_direction)
        elif direction is not None and direction not in directions:
            directions.append(direction)
        elif direction is None:
            invalid_values.append(value)
    return {
        "raw_value": raw,
        "directions": directions,
        "invalid_values": invalid_values,
        "error": bool(invalid_values),
    }


def _symbol_diagnostics():
    """Report optional symbol/linkage diagnostic tools for the host."""
    system = platform.system()
    tools = {
        "nm": shutil.which("nm"),
    }
    if system == "Darwin":
        tools["otool"] = shutil.which("otool")
    elif system == "Linux":
        tools["readelf"] = shutil.which("readelf")
    return {
        "platform": system,
        "tools": tools,
    }


def _runtime_loader_var():
    """Return the runtime library-path environment variable for the host."""
    system = platform.system()
    if system == "Darwin":
        return "DYLD_LIBRARY_PATH"
    if system == "Linux":
        return "LD_LIBRARY_PATH"
    return None


def _tool_discovery_record(name, path, override_env, dirs, probes):
    """Return path, override, availability, and command-output details."""
    record = {
        "name": name,
        "path": path,
        "override_env": override_env,
        "override_used": _tool_override_used(override_env),
        "available": _tool_available(path),
        "probes": {},
    }
    if not record["available"]:
        return record

    env = os.environ.copy()
    for probe_name, args in probes:
        result = _command_result(
            name + "_" + probe_name,
            [path] + args,
            dirs["base"],
            env,
            dirs["logs"] / (name + "_" + probe_name + ".json"))
        record["probes"][probe_name] = {
            "command": result["command"],
            "returncode": result["returncode"],
            "stdout_first_line": (
                result["stdout"].splitlines()[0]
                if result["stdout"].splitlines() else ""
            ),
            "stderr_first_line": (
                result["stderr"].splitlines()[0]
                if result["stderr"].splitlines() else ""
            ),
            "log": result["log"],
        }
    return record


def _cross_environment_report(tools, dirs):
    """Build the machine-readable cross-test environment report.

    12A is intentionally about discovery and reporting only.  The report
    records enough wrapper, launcher, path, loader, and direction state
    for 12B to reuse when it starts compiling with one implementation and
    running against the other.
    """
    open_mpi = tools["open_mpi"]
    mpich = tools["mpich"]
    cross = tools["cross"]
    directions = _cross_direction_summary(tools)
    report = {
        "platform": cross["platform"],
        "runtime_loader": cross["runtime_loader"],
        "directions": cross["directions"],
        "direction_raw_value": cross["direction_raw_value"],
        "direction_invalid_values": cross["direction_invalid_values"],
        "direction_details": directions,
        "discovery": tools.get("discovery", {}),
        "mpich_device": mpich.get("device"),
        "mpich_transport_env": cross["mpich_transport_env"],
        "paths": {
            "open_mpi": cross["open_mpi_paths"],
            "mpich": cross["mpich_paths"],
        },
        "tools": {
            "open_mpi_mpicc_abi": _tool_discovery_record(
                "open_mpi_mpicc_abi",
                open_mpi["mpicc_abi"],
                "OMPI_ABI_TEST_MPICC_ABI",
                dirs,
                [
                    ("version", ["--showme:version"]),
                    ("compile", ["--showme:compile"]),
                    ("link", ["--showme:link"]),
                    ("incdirs", ["--showme:incdirs"]),
                    ("libdirs", ["--showme:libdirs"]),
                    ("libs", ["--showme:libs"]),
                ]),
            "open_mpi_mpirun": _tool_discovery_record(
                "open_mpi_mpirun",
                open_mpi["mpirun"],
                "OMPI_ABI_TEST_MPIRUN",
                dirs,
                [("version", ["--version"])]),
            "mpich_mpicc_abi": _tool_discovery_record(
                "mpich_mpicc_abi",
                mpich["mpicc"],
                "MPICH_ABI_TEST_MPICC",
                dirs,
                [
                    ("version", ["--version"]),
                    ("show", ["-show"]),
                    ("compile_info", ["-compile-info"]),
                    ("link_info", ["-link-info"]),
                ]),
            "mpich_mpirun": _tool_discovery_record(
                "mpich_mpirun",
                mpich["mpirun"],
                "MPICH_ABI_TEST_MPIRUN",
                dirs,
                [("version", ["--version"])]),
        },
    }
    return report


# Per-direction compile/run roles for the two standard ABI cross paths.
# Each entry is (compile_implementation, run_implementation); the compile
# wrapper and the run-side paths/launcher are then looked up by role so the
# two directions share one dict-building path instead of duplicated branches.
_CROSS_DIRECTION_ROLES = {
    "mpich_compile_open_mpi_run": ("mpich", "open_mpi"),
    "open_mpi_compile_mpich_run": ("open_mpi", "mpich"),
}

# The ABI compile wrapper each implementation exposes under tools[impl].
_CROSS_COMPILE_WRAPPER_KEY = {
    "mpich": "mpicc",
    "open_mpi": "mpicc_abi",
}

# The tools["cross"] paths block that describes each implementation.
_CROSS_PATHS_KEY = {
    "mpich": "mpich_paths",
    "open_mpi": "open_mpi_paths",
}


def _cross_direction_summary(tools):
    """Describe the compile/run roles selected for each cross direction."""
    cross = tools["cross"]
    runtime_loader = cross["runtime_loader"]
    directions = {}
    for direction in cross["directions"]:
        roles = _CROSS_DIRECTION_ROLES.get(direction)
        if roles is None:
            continue
        compile_impl, run_impl = roles
        run_paths = cross[_CROSS_PATHS_KEY[run_impl]]
        directions[direction] = {
            "compile_implementation": compile_impl,
            "compile_wrapper": tools[compile_impl][
                _CROSS_COMPILE_WRAPPER_KEY[compile_impl]],
            "run_implementation": run_impl,
            "launcher": tools[run_impl]["mpirun"],
            "runtime_loader": runtime_loader,
            "runtime_library_path": run_paths["library"],
            "runtime_loader_policy": _cross_runtime_loader_policy(
                run_paths["library"], runtime_loader),
            "launcher_args": run_paths["launcher_args"],
        }
    return directions


def _cross_native_sanity_details(tools):
    """Describe native ABI compile/run checks for both implementations."""
    return {
        "open_mpi": {
            "implementation": "open_mpi",
            "compile_wrapper": tools["open_mpi"]["mpicc_abi"],
            "launcher": tools["open_mpi"]["mpirun"],
            "library_path": tools["cross"]["open_mpi_paths"]["library"],
            "runtime_loader": tools["cross"]["runtime_loader"],
            "launcher_args": tools["cross"]["open_mpi_paths"][
                "launcher_args"],
        },
        "mpich": {
            "implementation": "mpich",
            "compile_wrapper": tools["mpich"]["mpicc"],
            "launcher": tools["mpich"]["mpirun"],
            "library_path": tools["cross"]["mpich_paths"]["library"],
            "runtime_loader": tools["cross"]["runtime_loader"],
            "launcher_args": tools["cross"]["mpich_paths"]["launcher_args"],
        },
    }


def _cross_native_sanity_source():
    """Return the C source for native ABI runtime sanity checks."""
    return """\
#include "mpi.h"

#ifndef MPI_H_ABI
#error "standard ABI mpi.h was not used"
#endif

int main(int argc, char **argv)
{
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    return 0;
}
"""


def _cross_native_sanity_check(tools, dirs, implementation, details,
                               progress=None):
    """Compile and run one implementation against its own ABI runtime.

    Cross-implementation failures are only meaningful if each selected
    MPI implementation can first run the smallest ABI program with its
    own wrapper, launcher, and library path.  This preflight turns a
    broken local MPICH/Open MPI install or launcher environment into a
    single prerequisite failure instead of many timeout-looking cross
    failures.
    """
    check_name = "cross_native_" + implementation + "_init_finalize"
    if progress is not None:
        progress.start(check_name)

    source = dirs["src"] / (check_name + ".c")
    executable = dirs["bin"] / check_name
    _write_text(source, _cross_native_sanity_source())

    compile_command = (
        [details["compile_wrapper"]] +
        _cross_compile_overrides(tools, implementation) +
        [str(source), "-o", str(executable)]
    )
    compile_result = _command_result(
        "compile_" + check_name,
        compile_command,
        dirs["base"],
        os.environ.copy(),
        dirs["logs"] / ("compile_" + check_name + ".json"))
    if compile_result["returncode"] != 0:
        return _fail(
            check_name,
            "native MPI ABI Init/Finalize sanity compile failed",
            phase="compile",
            implementation=implementation,
            source=str(source),
            executable=str(executable),
            command=compile_result["command"],
            returncode=compile_result["returncode"],
            log=compile_result["log"])

    run_env = _runtime_env_for_implementation(
        tools, implementation, details["library_path"],
        details["runtime_loader"], replace=True)
    run_command = (
        [details["launcher"]] +
        _cross_launcher_args(details) +
        ["-n", "1", str(executable)]
    )
    run_result = _command_result(
        "run_" + check_name,
        run_command,
        dirs["base"],
        run_env,
        dirs["logs"] / ("run_" + check_name + ".json"))
    if run_result["returncode"] != 0:
        return _fail(
            check_name,
            "native MPI ABI Init/Finalize sanity run failed",
            phase="run",
            implementation=implementation,
            source=str(source),
            executable=str(executable),
            command=run_result["command"],
            returncode=run_result["returncode"],
            timed_out=run_result["timed_out"],
            timeout=run_result["timeout"],
            compile_log=compile_result["log"],
            run_log=run_result["log"])

    return _pass(
        check_name,
        phase="run",
        implementation=implementation,
        source=str(source),
        executable=str(executable),
        compile_command=compile_result["command"],
        run_command=run_result["command"],
        compile_log=compile_result["log"],
        run_log=run_result["log"])


def _cross_required_native_sanity_implementations(directions):
    """Return implementations that participate in selected directions."""
    required = set()
    for details in directions.values():
        required.add(details["compile_implementation"])
        required.add(details["run_implementation"])
    return sorted(required)


def _cross_native_sanity_checks(tools, dirs, directions, progress=None):
    """Run native ABI launcher sanity checks for selected implementations.

    Direction filtering is meant to let operators test one cross path at
    a time, so do not require implementations that are absent from the
    selected directions.  For every implementation that does participate,
    check its own wrapper, launcher, and ABI runtime together before
    cross-launching anything.  This keeps a broken local Open MPI or
    MPICH install from being misdiagnosed as a cross-ABI failure.
    """
    checks = []
    required = set(_cross_required_native_sanity_implementations(directions))
    for implementation, details in _cross_native_sanity_details(
            tools).items():
        if implementation not in required:
            continue
        _append_check(checks, _cross_native_sanity_check(
            tools, dirs, implementation, details, progress), progress)
    return checks


def _cross_direction_header_check(manifest, tools, direction, details,
                                  progress=None):
    """Return compile-side ABI header declarations for one direction."""
    check_name = "cross_direction_" + direction + "_probe_generation"
    header = _cross_compile_header(tools, details["compile_implementation"])
    if header is None:
        if progress is not None:
            progress.start(check_name)
        return None, None, [_fail(
            check_name,
            "cross compile-side ABI header is unavailable",
            compile_implementation=details["compile_implementation"],
            compile_wrapper=details["compile_wrapper"])]

    declared_names = _parse_header_constant_names(header)
    include_fortran = _fortran_bindings_enabled(manifest)
    if progress is not None:
        progress.start(check_name)
    generation_check = _runtime_probe_generation_check(
        manifest, include_fortran, declared_names, check_name)
    checks = [generation_check]
    if generation_check["result"] != "PASS":
        return header, None, checks

    expected_names = _runtime_probe_constant_names(
        manifest, include_fortran, declared_names)
    missing_names = sorted(expected_names - declared_names)
    constants_check_name = (
        "cross_direction_" + direction + "_metadata_constants"
    )
    if progress is not None:
        progress.start(constants_check_name)
    if missing_names:
        checks.append(_fail(
            constants_check_name,
            "cross compile-side ABI header is missing probe constants",
            header=str(header),
            compile_implementation=details["compile_implementation"],
            missing=missing_names[:20],
            missing_count=len(missing_names),
            expected_count=len(expected_names),
            declared_count=len(declared_names)))
        return header, None, checks

    checks.append(_pass(
        constants_check_name,
        header=str(header),
        compile_implementation=details["compile_implementation"],
        checked=len(expected_names),
        declared_count=len(declared_names)))
    return header, declared_names, checks


def _run_cross_direction_probe_cases(srcdir, manifest, tools, dirs,
                                     direction, details, declared_names,
                                     cases, progress=None):
    """Compile and launch ABI probes for one cross-implementation direction."""
    checks = []
    compile_wrapper = details["compile_wrapper"]
    launcher = details["launcher"]
    compile_implementation = details["compile_implementation"]
    run_implementation = details["run_implementation"]
    runtime_library_path = details["runtime_library_path"]
    runtime_loader = details["runtime_loader"]
    compile_env = os.environ.copy()
    run_env = _runtime_env_for_implementation(
        tools, run_implementation, runtime_library_path, runtime_loader,
        replace=True)
    launcher_args = _cross_launcher_args(details)
    compile_overrides = _cross_compile_overrides(
        tools, compile_implementation)
    compile_extra_flags = _cross_compile_extra_flags(details)

    for case in cases:
        name = direction + "_" + case["name"]
        check_name = "cross_probe_" + name
        if (case.get("requires_fortran")
                and not _fortran_bindings_enabled(manifest)):
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _skip(
                check_name,
                SKIP_FORTRAN_BINDINGS_DISABLED,
                phase="configure",
                direction=direction), progress)
            continue
        feature = case.get("requires_feature")
        if feature is not None:
            feature_info = _optional_feature_info(manifest, feature)
            if feature_info["enabled"] is False:
                if progress is not None:
                    progress.start(check_name)
                _append_check(checks, _skip(
                    check_name,
                    _optional_feature_skip_reason(feature),
                    phase="configure",
                    direction=direction,
                    feature=feature,
                    feature_state=feature_info), progress)
                continue

        rank_count = tools["rank_counts"]["np{0}".format(
            case["rank_count"])]
        source = dirs["src"] / (name + ".c")
        executable = dirs["bin"] / name
        try:
            body = _prepare_installed_c_probe_body(
                srcdir, case, manifest, declared_names)
            probe_source = _c_probe_source(srcdir, case, body)
        except RuntimeError as exc:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _fail(
                check_name,
                "cross C ABI probe source is unavailable",
                phase="source",
                direction=direction,
                error=str(exc)), progress)
            continue
        _write_text(source, probe_source)

        # Context common to every result record for this case.  Splatting
        # this into each branch keeps the per-case identity fields in one
        # place so no branch can silently omit one of them.
        base = {
            "direction": direction,
            "compile_implementation": compile_implementation,
            "run_implementation": run_implementation,
            "source": str(source),
            "executable": str(executable),
        }

        compile_command = (
            [compile_wrapper] + compile_overrides + compile_extra_flags +
            [str(source), "-o", str(executable)]
        )
        if progress is not None:
            progress.start(check_name)
        compile_result = _command_result(
            "compile_cross_" + name,
            compile_command,
            dirs["base"],
            compile_env,
            dirs["logs"] / ("compile_cross_" + name + ".json"))
        if compile_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "cross C ABI probe compile failed",
                phase="compile",
                command=compile_result["command"],
                returncode=compile_result["returncode"],
                log=compile_result["log"],
                **base), progress)
            continue

        rewrite_result = _cross_verify_or_rewrite_abi_libraries(
            executable, dirs, run_env, name, runtime_library_path)
        if rewrite_result["result"] != "PASS":
            detail = dict(
                base,
                phase="runtime_linkage",
                compile_log=compile_result["log"],
                runtime_linkage=rewrite_result["details"])
            if rewrite_result["result"] == "SKIP":
                _append_check(checks, _skip(
                    check_name,
                    rewrite_result["skip_reason"],
                    **detail), progress)
            else:
                _append_check(checks, _fail(
                    check_name,
                    rewrite_result["details"]["message"],
                    **detail), progress)
            continue

        run_command = (
            [launcher] + launcher_args +
            ["-n", str(rank_count), str(executable)]
        )
        run_result = _command_result(
            "run_cross_" + name,
            run_command,
            dirs["base"],
            run_env,
            dirs["logs"] / ("run_cross_" + name + ".json"))
        if run_result["timed_out"]:
            _append_check(checks, _fail(
                check_name,
                "cross C ABI probe runtime timed out",
                phase="run",
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                timeout=run_result["timeout"],
                timed_out=run_result["timed_out"],
                compile_log=compile_result["log"],
                run_log=run_result["log"],
                runtime_linkage=rewrite_result["details"],
                **base), progress)
            continue
        skip_reason = case.get("skip_exit_codes", {}).get(
            run_result["returncode"])
        if skip_reason is not None:
            _append_check(checks, _skip(
                check_name,
                skip_reason,
                phase="run",
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"],
                **base), progress)
            continue
        # A known limitation of the *runtime* implementation, rather than a
        # regression on our side: record it as an expected failure so it stays
        # visible without failing the suite.
        xfail_reason = case.get(
            "xfail_run_implementation_exit_codes", {}).get(
                run_implementation, {}).get(run_result["returncode"])
        if xfail_reason is not None:
            _append_check(checks, _xfail(
                check_name,
                xfail_reason,
                "cross C ABI probe hit a known {0} limitation".format(
                    run_implementation),
                phase="run",
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"],
                runtime_linkage=rewrite_result["details"],
                **base), progress)
            continue
        if run_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "cross C ABI probe runtime failed",
                phase="run",
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"],
                runtime_linkage=rewrite_result["details"],
                **base), progress)
            continue

        _append_check(checks, _pass(
            check_name,
            phase="run",
            rank_count=rank_count,
            compile_command=compile_result["command"],
            run_command=run_result["command"],
            compile_log=compile_result["log"],
            run_log=run_result["log"],
            runtime_linkage=rewrite_result["details"],
            **base), progress)

    return checks


def _cross_direction_result(direction, details, preflight_checks,
                            probe_checks):
    """Return one aggregate per-direction cross probe result."""
    probe_counts = _check_counts(probe_checks)
    preflight_counts = _check_counts(preflight_checks)
    result = "PASS"
    if probe_counts.get("FAIL", 0) or preflight_counts.get("FAIL", 0):
        result = "FAIL"
    elif probe_counts.get("PASS", 0) == 0:
        result = "SKIP"
    payload = {
        "direction": direction,
        "details": details,
        "preflight_status": preflight_counts,
        "probe_status": probe_counts,
        "preflight_count": len(preflight_checks),
        "probe_count": len(probe_checks),
    }
    name = "cross_direction_" + direction + "_summary"
    if result == "FAIL":
        return _fail(name, "cross direction probes failed", **payload)
    if result == "SKIP":
        return _skip(name, SKIP_CROSS_PROBES_NOT_PASSED, **payload)
    return _pass(name, **payload)


def _cross_summary_result(direction_results, directions):
    """Return the aggregate cross-direction execution result."""
    counts = _check_counts(direction_results)
    payload = {
        "directions": directions,
        "direction_status": counts,
    }
    if counts.get("FAIL", 0):
        return _fail(
            "cross_direction_summary",
            "one or more cross directions failed",
            **payload)
    if counts.get("PASS", 0):
        return _pass("cross_direction_summary", **payload)
    return _skip(
        "cross_direction_summary",
        SKIP_CROSS_PROBES_NOT_PASSED,
        **payload)


def run_cross_checks(manifest, srcdir, tools, outdir, progress=None):
    """Run cross-implementation ABI checks.

    The MPICH mode reuses the same generated executable-per-case model
    as check-abi, but compiles with one implementation's MPI Forum ABI
    wrapper and launches against the other implementation's ABI runtime.
    Preflight checks keep header, wrapper, optional-feature, and probe
    table drift separate from genuine cross-runtime ABI failures.
    """
    dirs = _cross_test_dirs(outdir)
    checks = []
    if progress is not None:
        progress.start("cross implementation environment")

    environment = _cross_environment_report(tools, dirs)
    directions = _cross_direction_summary(tools)
    _append_check(checks, _pass(
        "cross_environment",
        environment=environment,
        direction_count=len(directions)), progress)

    native_checks = _cross_native_sanity_checks(
        tools, dirs, directions, progress)
    checks.extend(native_checks)
    if any(check["result"] == "FAIL" for check in native_checks):
        _append_check(checks, _fail(
            "cross_native_sanity_summary",
            "native MPI ABI Init/Finalize sanity checks failed",
            check_status=_check_counts(native_checks)), progress)
        return checks, environment

    if progress is not None:
        progress.start("cross_linkage_diagnostics_available")
    _append_check(checks, _cross_linkage_diagnostic_check(), progress)

    header_checks = _cross_header_semantic_checks(
        srcdir, manifest, tools, progress)
    checks.extend(header_checks)

    wrapper_checks = {}
    for implementation in ("open_mpi", "mpich"):
        if progress is not None:
            progress.start("cross_wrapper_" + implementation + "_intent")
        wrapper_check = _cross_wrapper_intent_check(
            tools, dirs, implementation, {
                "compile_wrapper": (
                    tools["open_mpi"]["mpicc_abi"]
                    if implementation == "open_mpi"
                    else tools["mpich"]["mpicc"]
                )
            })
        wrapper_checks[implementation] = wrapper_check
        _append_check(checks, wrapper_check, progress)

    direction_results = []
    for direction, details in directions.items():
        preflight_checks = [
            wrapper_checks[details["compile_implementation"]],
            wrapper_checks[details["run_implementation"]],
        ]
        header, declared_names, header_preflight = _cross_direction_header_check(
            manifest, tools, direction, details, progress)
        preflight_checks.extend(header_preflight)
        _extend_checks(checks, header_preflight, progress)
        probe_checks = []
        if declared_names is not None:
            abi_probe_checks = _run_cross_direction_probe_cases(
                srcdir, manifest, tools, dirs, direction, details,
                declared_names, INSTALLED_C_ABI_PROBES, progress)
            probe_checks.extend(abi_probe_checks)

            runtime_check_name = (
                "cross_direction_" + direction +
                "_runtime_api_probe_generation"
            )
            if progress is not None:
                progress.start(runtime_check_name)
            runtime_generation = _runtime_api_probe_generation_check(
                srcdir, manifest, header, INSTALLED_C_RUNTIME_API_PROBES,
                runtime_check_name)
            _append_check(checks, runtime_generation, progress)
            preflight_checks.append(runtime_generation)
            if runtime_generation["result"] == "PASS":
                runtime_probe_checks = _run_cross_direction_probe_cases(
                    srcdir, manifest, tools, dirs, direction, details,
                    declared_names, INSTALLED_C_RUNTIME_API_PROBES, progress)
                probe_checks.extend(runtime_probe_checks)

            callback_check_name = (
                "cross_direction_" + direction +
                "_callback_api_probe_generation"
            )
            if progress is not None:
                progress.start(callback_check_name)
            callback_generation = _runtime_api_probe_generation_check(
                srcdir, manifest, header, INSTALLED_C_CALLBACK_PROBES,
                callback_check_name)
            _append_check(checks, callback_generation, progress)
            preflight_checks.append(callback_generation)
            callback_audit_name = (
                "cross_direction_" + direction +
                "_callback_api_coverage_audit"
            )
            if progress is not None:
                progress.start(callback_audit_name)
            callback_audit = _callback_api_coverage_audit(
                manifest, header, INSTALLED_C_CALLBACK_PROBES,
                callback_audit_name,
                tolerated_legacy_names=(
                    MPI_REMOVED_LEGACY_C_NAMES
                    if details["compile_implementation"] == "mpich"
                    else set()
                ))
            _append_check(checks, callback_audit, progress)
            preflight_checks.append(callback_audit)
            if callback_generation["result"] == "PASS":
                callback_probe_checks = _run_cross_direction_probe_cases(
                    srcdir, manifest, tools, dirs, direction, details,
                    declared_names, INSTALLED_C_CALLBACK_PROBES, progress)
                probe_checks.extend(callback_probe_checks)

            checks.extend(probe_checks)
        direction_result = _cross_direction_result(
            direction, details, preflight_checks, probe_checks)
        direction_results.append(direction_result)
        _append_check(checks, direction_result, progress)

    _append_check(checks, _cross_summary_result(
        direction_results, directions), progress)
    return checks, environment
