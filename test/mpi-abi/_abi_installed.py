#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Installed ``check-abi`` framework: compile/link/run,
header/symbol/wrapper checks, and C/Fortran probe
execution."""

import ast
import os
from pathlib import Path
import platform
import re
import shlex
import shutil
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    CLASS_IMPLEMENTED, FORTRAN_BINDING_LANGUAGES,
    MIN_EXPECTED_C_HEADER_PROTOTYPES, MPI_REMOVED_LEGACY_CONSTANT_NAMES,
    MPI_REMOVED_LEGACY_C_NAMES, SKIP_FORTRAN_BINDINGS_DISABLED,
    SKIP_FORTRAN_BINDING_DISABLED, SKIP_FORTRAN_OPTIONAL_DATATYPES_DEFERRED,
    SKIP_FORTRAN_WRAPPER_UNAVAILABLE, SKIP_HEADER_UNAVAILABLE,
    SKIP_LINKAGE_INSPECTION_UNAVAILABLE, SKIP_SYMBOL_DIAGNOSTICS_UNAVAILABLE,
    _append_check, _command_timeout, _fail, _pass, _probe_prologue_text,
    _read_text, _skip, _write_json, _write_text)
from _abi_tables import (
    INSTALLED_C_ABI_PROBES, INSTALLED_C_CALLBACK_PROBES,
    INSTALLED_C_RUNTIME_API_PROBES, INSTALLED_FORTRAN_COMPILE_PROBES,
    INSTALLED_FORTRAN_RUNTIME_PROBES)
from _abi_discovery import (
    _flag_dirs_from_words, _tool_available, _words_define_macro,
    _words_link_library)
from _abi_probes import (
    _callback_api_coverage_audit, _fortran_bindings_enabled,
    _metadata_integer_value, _normalize_c_signature_text,
    _optional_feature_info, _optional_feature_skip_reason,
    _parse_c_header_prototypes, _parse_header_constant_names,
    _parse_header_constants, _prepare_installed_c_probe_body,
    _remove_c_comments, _runtime_api_coverage_audit,
    _runtime_api_probe_generation_check, _runtime_probe_constant_names,
    _runtime_probe_generation_check)


def _count_by(entries, key):
    counts = {}
    for entry in entries:
        value = entry.get(key)
        counts[value] = counts.get(value, 0) + 1
    return counts


def _language_counts(entries):
    counts = {
        "c": 0,
        "mpif.h": 0,
        "use mpi": 0,
        "use mpi_f08": 0,
    }
    for entry in entries:
        for language, enabled in entry.get("languages", {}).items():
            if enabled:
                counts[language] += 1
    return counts


def _check_counts(checks):
    return _count_by(checks, "result")


def _command_result(name, command, cwd, env, log_path):
    """Run one subprocess, capture output, and write a JSON command log."""
    log_path.parent.mkdir(parents=True, exist_ok=True)
    timeout = _command_timeout()
    try:
        completed = subprocess.run(
            command,
            cwd=str(cwd),
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
            timeout=timeout)
        return_code = completed.returncode
        stdout = completed.stdout
        stderr = completed.stderr
        timed_out = False
    except subprocess.TimeoutExpired as exc:
        return_code = 124
        stdout = exc.stdout or ""
        stderr = exc.stderr or ""
        if isinstance(stdout, bytes):
            stdout = stdout.decode("utf-8", errors="replace")
        if isinstance(stderr, bytes):
            stderr = stderr.decode("utf-8", errors="replace")
        stderr += "\ncommand timed out after {0} seconds".format(timeout)
        timed_out = True
    except OSError as exc:
        return_code = 127
        stdout = ""
        stderr = str(exc)
        timed_out = False

    log = {
        "name": name,
        "command": command,
        "cwd": str(cwd),
        "environment": _logged_environment(env),
        "returncode": return_code,
        "timeout": timeout,
        "timed_out": timed_out,
        "stdout": stdout,
        "stderr": stderr,
    }
    _write_json(log_path, log)
    return {
        "command": command,
        "returncode": return_code,
        "timeout": timeout,
        "timed_out": timed_out,
        "stdout": stdout,
        "stderr": stderr,
        "log": str(log_path),
    }


def _logged_environment(env):
    """Return the diagnostic environment subset recorded in command logs."""
    prefixes = ("FI_", "UCX_", "OMPI_ABI_TEST_", "MPICH_ABI_TEST_")
    names = {"LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH"}
    return {
        key: value for key, value in sorted(env.items())
        if key in names or key.startswith(prefixes)
    }


def _installed_test_env(tools):
    """Build the environment for installed wrapper/compiler/runtime checks."""
    return _runtime_loader_env(
        tools["paths"]["library"],
        tools["paths"]["runtime_loader"],
        replace=False)


def _runtime_loader_env(library_path, loader_var, replace=False):
    """Return an environment with controlled runtime library lookup.

    Normal installed checks prepend an override so users can layer the
    ABI install over their shell environment.  MPICH compatibility runs
    need stricter behavior: when switching a binary between
    implementations, stale LD_LIBRARY_PATH/DYLD_LIBRARY_PATH entries can
    silently load the wrong libmpi.  The replace mode is therefore the
    policy 12B should use for cross-direction runtime jobs.
    """
    env = os.environ.copy()
    if not loader_var:
        return env
    if library_path:
        if replace:
            env[loader_var] = library_path
        else:
            existing = env.get(loader_var)
            env[loader_var] = (
                library_path if not existing
                else library_path + os.pathsep + existing
            )
    elif replace:
        env.pop(loader_var, None)
    return env


def _host_ipv4_interfaces():
    """Return active host IPv4 interface names.

    The MPICH CH4:OFI sockets/tcp providers can select macOS tunnel
    interfaces such as utun*, which then fail in MPI_Finalize when OFI
    tries to flush self messages.  Keep interface discovery deliberately
    small and diagnostic: it is only used to pick a safer default for the
    local MPICH compatibility tests, and explicit environment variables
    remain available for unusual hosts.
    """
    try:
        result = subprocess.run(
            ["ifconfig"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
            timeout=_command_timeout())
    except (OSError, subprocess.TimeoutExpired):
        return []
    if result.returncode != 0:
        return []

    interfaces = []
    current = None
    for line in result.stdout.splitlines():
        if line and not line[0].isspace() and ":" in line:
            current = line.split(":", 1)[0]
        elif current and line.lstrip().startswith("inet "):
            interfaces.append(current)
    return interfaces


def _preferred_mpich_ofi_interface():
    """Return a likely usable interface for local MPICH OFI tests."""
    explicit = os.environ.get("MPICH_ABI_TEST_OFI_IFACE")
    if explicit:
        return explicit

    provider = _mpich_ofi_provider()
    provider_iface_env = {
        "tcp": "FI_TCP_IFACE",
        "sockets": "FI_SOCKETS_IFACE",
    }.get(provider)
    if provider_iface_env and os.environ.get(provider_iface_env):
        return os.environ[provider_iface_env]

    interfaces = _host_ipv4_interfaces()
    excluded_prefixes = ("lo", "utun", "awdl", "llw", "bridge", "gif",
                         "stf", "anpi")
    preferred = [
        iface for iface in interfaces
        if not iface.startswith(excluded_prefixes)
    ]
    for iface in preferred:
        if iface.startswith("en"):
            return iface
    return preferred[0] if preferred else None


def _mpich_ofi_provider():
    """Return the OFI provider selected for MPICH compatibility runs."""
    provider = os.environ.get("MPICH_ABI_TEST_OFI_PROVIDER")
    if provider is not None:
        return provider
    return "tcp"


def _mpich_ofi_runtime_env(mpich_device):
    """Return MPICH-specific OFI environment overrides.

    MPICH 5.0.x configured as ch4:ofi still uses libfabric even for
    one- and two-rank local tests.  On macOS libfabric may choose utun*
    tunnel interfaces by default; forcing tcp/sockets to a real local
    interface avoids turning local launcher setup into unrelated ABI
    test failures.  FI_PROVIDER=shm is not a safe default here because
    this MPICH/libfabric combination rejects it for the stream endpoint
    requested by CH4:OFI.
    """
    if mpich_device != "ch4:ofi":
        return {}

    provider = _mpich_ofi_provider()
    if provider in ("", "none", "system"):
        return {}

    env = {"FI_PROVIDER": provider}
    iface = _preferred_mpich_ofi_interface()
    if iface:
        if provider == "tcp":
            env["FI_TCP_IFACE"] = iface
        elif provider == "sockets":
            env["FI_SOCKETS_IFACE"] = iface
    return env


def _mpich_ucx_runtime_env(mpich_device):
    """Return MPICH-specific UCX environment overrides."""
    if mpich_device != "ch4:ucx":
        return {}

    tls = os.environ.get("MPICH_ABI_TEST_UCX_TLS", "self,sm")
    if tls in ("", "system"):
        return {}

    env = {"UCX_TLS": tls}
    net_devices = os.environ.get("MPICH_ABI_TEST_UCX_NET_DEVICES")
    if net_devices:
        env["UCX_NET_DEVICES"] = net_devices
    return env


def _mpich_transport_runtime_env(mpich_device):
    """Return MPICH-device-specific runtime environment overrides."""
    env = {}
    env.update(_mpich_ofi_runtime_env(mpich_device))
    env.update(_mpich_ucx_runtime_env(mpich_device))
    return env


def _runtime_env_for_implementation(tools, implementation, library_path,
                                    loader_var, replace=False):
    """Return a launch environment for one selected implementation."""
    env = _runtime_loader_env(library_path, loader_var, replace=replace)
    if implementation == "mpich":
        env.update(tools.get("cross", {}).get("mpich_transport_env", {}))
    return env


def _cross_runtime_loader_policy(library_path, loader_var):
    """Describe the MPICH compatibility runtime-loader policy."""
    return {
        "mode": "replace",
        "runtime_loader": loader_var,
        "runtime_library_path": library_path,
        "preserves_existing_loader_path": False,
    }


def _test_dirs(outdir, name):
    """Create and return a {base,src,bin,logs} scratch directory layout."""
    base = outdir / name
    dirs = {
        "base": base,
        "src": base / "src",
        "bin": base / "bin",
        "logs": base / "logs",
    }
    for path in dirs.values():
        path.mkdir(parents=True, exist_ok=True)
    return dirs


def _installed_test_dirs(outdir):
    """Create and return the installed-test scratch directory layout."""
    return _test_dirs(outdir, "installed")


def _cross_test_dirs(outdir):
    """Create and return the cross-test scratch directory layout."""
    return _test_dirs(outdir, "cross")


def _split_launcher_args(args):
    """Split a launcher-argument string into a list (empty when unset)."""
    return shlex.split(args) if args else []


def _launcher_args(tools):
    """Return optional extra mpirun arguments from the tool configuration."""
    return _split_launcher_args(tools["paths"]["launcher_args"])


def _include_library_flags(include_path, library_path):
    """Return -I/-L compiler flags for the given include/library paths."""
    args = []
    if include_path:
        args.append("-I" + include_path)
    if library_path:
        args.append("-L" + library_path)
    return args


def _compile_overrides(tools):
    """Return optional include/library overrides for installed C probes."""
    paths = tools["paths"]
    return _include_library_flags(paths["include"], paths["library"])


def _cross_paths_for_implementation(tools, implementation):
    """Return cross include/library/launcher paths for one implementation."""
    key = implementation + "_paths"
    return tools["cross"][key]


def _cross_compile_overrides(tools, implementation):
    """Return include/library overrides for the compile-side wrapper."""
    paths = _cross_paths_for_implementation(tools, implementation)
    return _include_library_flags(paths.get("include"), paths.get("library"))


def _cross_compile_extra_flags(details):
    """Return direction-specific extra compile/link flags.

    Do not add --as-needed, -dead_strip_dylibs, or equivalent flags for
    non-standard MPI ABI libraries.  MPI-5.0 section 21.2.1 says ABI
    application binaries must not require more than mpi_abi as the sole
    direct MPI ABI dependency, and section 16.2.1 still requires PMPI
    alternate entry points for MPI routines.  The harness should only
    reason about the standard ABI library, libmpi_abi; non-standard
    extra libraries are not part of the MPI ABI test contract.  See Open
    MPI issue #13955.
    """
    return []


def _cross_launcher_args(details):
    """Return launcher arguments for one cross-test direction."""
    return _split_launcher_args(details.get("launcher_args"))


def _installed_helper_unit_checks():
    """Fast self-check for the shared installed-test helper functions."""
    failures = []
    flag_cases = (
        (("/inc", "/lib"), ["-I/inc", "-L/lib"]),
        (("", "/lib"), ["-L/lib"]),
        (("/inc", ""), ["-I/inc"]),
        ((None, None), []),
    )
    for (include_path, library_path), expected in flag_cases:
        got = _include_library_flags(include_path, library_path)
        if got != expected:
            failures.append({
                "helper": "_include_library_flags",
                "input": [include_path, library_path],
                "expected": expected,
                "got": got,
            })
    split_cases = (
        ("-x A=1 --bind-to none", ["-x", "A=1", "--bind-to", "none"]),
        ("", []),
        (None, []),
    )
    for text, expected in split_cases:
        got = _split_launcher_args(text)
        if got != expected:
            failures.append({
                "helper": "_split_launcher_args",
                "input": text,
                "expected": expected,
                "got": got,
            })
    if failures:
        return _fail("fast_installed_helper_unit_checks",
                     "installed helper unit checks failed",
                     failures=failures)
    return _pass("fast_installed_helper_unit_checks",
                 checked=len(flag_cases) + len(split_cases))


def _cross_compile_header(tools, implementation):
    """Return the ABI header used to generate one cross direction.

    Open MPI installs its MPI Forum ABI declarations as mpi.h under the
    standard_abi include directory.  MPICH installs a normal mpi.h that
    includes mpi_abi.h only when MPI_ABI is defined, so source parsing
    must use mpi_abi.h directly for MPICH compile-side generation.
    """
    include = _cross_paths_for_implementation(tools, implementation).get(
        "include")
    if not include:
        return None
    include_path = Path(include)
    if implementation == "mpich":
        candidates = (
            include_path / "mpi_abi.h",
            include_path / "standard_abi" / "mpi.h",
            include_path / "mpi.h",
        )
    else:
        candidates = (
            include_path / "mpi.h",
            include_path / "standard_abi" / "mpi.h",
            include_path / "mpi_abi.h",
        )
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return None


def _cross_header_constant_semantics_check(manifest, header, implementation):
    """Compare one cross implementation's ABI constants to metadata."""
    check_name = "cross_header_" + implementation + "_constant_semantics"
    header_constants, unparsed_header_constants = _parse_header_constants(
        header)
    missing = []
    mismatches = []
    unparsed = []
    checked = 0
    skipped = []
    for entry in manifest["constants"]:
        expected = _metadata_integer_value(entry["abi_value"])
        if expected is None:
            skipped.append(entry["name"])
            continue
        if entry["c_type"] is None:
            skipped.append(entry["name"])
            continue
        if entry["category"] == "DEPRECATED_FUNCS":
            skipped.append(entry["name"])
            continue
        checked += 1
        name = entry["name"]
        if name not in header_constants:
            if name in unparsed_header_constants:
                unparsed.append(name)
            else:
                missing.append(name)
        elif header_constants[name] != expected:
            mismatches.append({
                "name": name,
                "expected": expected,
                "actual": header_constants[name],
            })

    if checked == 0:
        return _fail(
            check_name,
            "no cross ABI header constants were validated",
            header=str(header),
            implementation=implementation,
            checked=checked,
            skipped_count=len(skipped),
            parsed_header_count=len(header_constants),
            unparsed_header_count=len(unparsed_header_constants))

    if missing or mismatches or unparsed:
        return _fail(
            check_name,
            "cross ABI header constants differ from metadata",
            header=str(header),
            implementation=implementation,
            checked=checked,
            skipped_count=len(skipped),
            missing=missing[:20],
            missing_count=len(missing),
            unparsed=unparsed[:20],
            unparsed_count=len(unparsed),
            mismatches=mismatches[:20],
            mismatch_count=len(mismatches))

    return _pass(
        check_name,
        header=str(header),
        implementation=implementation,
        checked=checked,
        skipped_count=len(skipped))


def _cross_header_typedef_semantics_check(manifest, header, implementation):
    """Check that standard ABI typedef names used by metadata are declared."""
    check_name = "cross_header_" + implementation + "_typedef_semantics"
    typedefs = _parse_c_header_typedef_names(header)
    expected = {
        _normalize_c_typedef_reference(entry["c_type"])
        for entry in manifest["constants"]
        if isinstance(entry.get("c_type"), str) and
        entry["c_type"].startswith("MPI_")
    }
    expected.update({
        "MPI_Aint",
        "MPI_Count",
        "MPI_Offset",
    })
    missing = sorted(expected - typedefs)
    if missing:
        return _fail(
            check_name,
            "cross ABI header is missing expected MPI typedefs",
            header=str(header),
            implementation=implementation,
            missing=missing,
            typedef_count=len(typedefs),
            expected_count=len(expected))
    return _pass(
        check_name,
        header=str(header),
        implementation=implementation,
        checked=len(expected),
        typedef_count=len(typedefs))


def _cross_header_feature_set_check(headers, manifest, expected_signatures):
    """Compare Open MPI and MPICH ABI header API/constant availability."""
    check_name = "cross_header_feature_availability"
    expected_apis = set(expected_signatures)
    expected_constants = {
        entry["name"] for entry in manifest["constants"]
    }
    expected_typedefs = {
        _normalize_c_typedef_reference(entry["c_type"])
        for entry in manifest["constants"]
        if isinstance(entry.get("c_type"), str) and
        entry["c_type"].startswith("MPI_")
    }
    expected_typedefs.update({
        "MPI_Aint",
        "MPI_Count",
        "MPI_Offset",
    })
    prototypes = {
        implementation: set(_parse_c_header_prototypes(header)) &
        expected_apis
        for implementation, header in headers.items()
    }
    constants = {
        implementation: _parse_header_constant_names(header) &
        expected_constants
        for implementation, header in headers.items()
    }
    typedefs = {
        implementation: _parse_c_header_typedef_names(header) &
        expected_typedefs
        for implementation, header in headers.items()
    }
    open_mpi = "open_mpi"
    mpich = "mpich"
    # This is a declared MPI Forum ABI surface comparison, not a runtime
    # optional-feature probe.  The ABI header should expose the standard
    # ABI prototypes, typedefs, and constants that the metadata says are
    # in scope.  Runtime families such as MPI-IO, RMA, dynamic process,
    # and MPI_T events get stable configure/runtime skips in the actual
    # probe loop when a selected implementation cannot execute them.  If
    # one implementation's ABI header omits a standard declaration that
    # the other exposes, that is a header-level divergence.
    api_only_open_mpi = sorted(prototypes[open_mpi] - prototypes[mpich])
    api_only_mpich = sorted(
        (prototypes[mpich] - prototypes[open_mpi]) -
        MPI_REMOVED_LEGACY_C_NAMES
    )
    tolerated_api_only_mpich = sorted(
        (prototypes[mpich] - prototypes[open_mpi]) &
        MPI_REMOVED_LEGACY_C_NAMES
    )
    constants_only_open_mpi = sorted(constants[open_mpi] - constants[mpich])
    constants_only_mpich = sorted(
        (constants[mpich] - constants[open_mpi]) -
        MPI_REMOVED_LEGACY_CONSTANT_NAMES
    )
    tolerated_constants_only_mpich = sorted(
        (constants[mpich] - constants[open_mpi]) &
        MPI_REMOVED_LEGACY_CONSTANT_NAMES
    )
    typedefs_only_open_mpi = sorted(typedefs[open_mpi] - typedefs[mpich])
    typedefs_only_mpich = sorted(typedefs[mpich] - typedefs[open_mpi])
    if (api_only_open_mpi or api_only_mpich or
            constants_only_open_mpi or constants_only_mpich or
            typedefs_only_open_mpi or typedefs_only_mpich):
        return _fail(
            check_name,
            "Open MPI and MPICH ABI headers expose different features",
            api_only_open_mpi=api_only_open_mpi[:20],
            api_only_open_mpi_count=len(api_only_open_mpi),
            api_only_mpich=api_only_mpich[:20],
            api_only_mpich_count=len(api_only_mpich),
            tolerated_api_only_mpich=tolerated_api_only_mpich,
            tolerated_api_only_mpich_count=len(tolerated_api_only_mpich),
            constants_only_open_mpi=constants_only_open_mpi[:20],
            constants_only_open_mpi_count=len(constants_only_open_mpi),
            constants_only_mpich=constants_only_mpich[:20],
            constants_only_mpich_count=len(constants_only_mpich),
            tolerated_constants_only_mpich=tolerated_constants_only_mpich,
            tolerated_constants_only_mpich_count=len(
                tolerated_constants_only_mpich),
            typedefs_only_open_mpi=typedefs_only_open_mpi[:20],
            typedefs_only_open_mpi_count=len(typedefs_only_open_mpi),
            typedefs_only_mpich=typedefs_only_mpich[:20],
            typedefs_only_mpich_count=len(typedefs_only_mpich))
    return _pass(
        check_name,
        api_count=len(prototypes[open_mpi]),
        constant_count=len(constants[open_mpi]),
        typedef_count=len(typedefs[open_mpi]),
        tolerated_api_only_mpich=tolerated_api_only_mpich,
        tolerated_api_only_mpich_count=len(tolerated_api_only_mpich),
        tolerated_constants_only_mpich=tolerated_constants_only_mpich,
        tolerated_constants_only_mpich_count=len(
            tolerated_constants_only_mpich))


def _cross_header_semantic_checks(srcdir, manifest, tools, progress=None):
    """Run parsed semantic checks on both selected cross ABI headers."""
    checks = []
    headers = {}
    expected_signatures, excluded_names = _standard_abi_expected_signatures(
        srcdir)
    for implementation in ("open_mpi", "mpich"):
        tolerated_legacy_names = (
            MPI_REMOVED_LEGACY_C_NAMES if implementation == "mpich"
            else set()
        )
        check_prefix = "cross_header_" + implementation
        header = _cross_compile_header(tools, implementation)
        if progress is not None:
            progress.start(check_prefix + "_available")
        if header is None:
            _append_check(checks, _fail(
                check_prefix + "_available",
                "cross ABI header is unavailable",
                implementation=implementation), progress)
            continue
        headers[implementation] = header
        _append_check(checks, _pass(
            check_prefix + "_available",
            implementation=implementation,
            header=str(header)), progress)

        prototypes = _parse_c_header_prototypes(header)
        if progress is not None:
            progress.start(check_prefix + "_prototype_parse")
        if len(prototypes) < MIN_EXPECTED_C_HEADER_PROTOTYPES:
            _append_check(checks, _fail(
                check_prefix + "_prototype_parse",
                "cross ABI header prototype parser found too few entries",
                implementation=implementation,
                header=str(header),
                prototype_count=len(prototypes),
                minimum_expected=MIN_EXPECTED_C_HEADER_PROTOTYPES),
                progress)
            continue
        _append_check(checks, _pass(
            check_prefix + "_prototype_parse",
            implementation=implementation,
            header=str(header),
            prototype_count=len(prototypes)), progress)

        if progress is not None:
            progress.start(check_prefix + "_signature_semantics")
        _append_check(checks, _signature_comparison_check(
            prototypes, expected_signatures,
            check_prefix + "_signature_semantics",
            compare_parameter_names=False,
            tolerated_extra_names=tolerated_legacy_names), progress)

        if progress is not None:
            progress.start(check_prefix + "_non_abi_absence")
        _append_check(checks, _non_abi_absence_check(
            prototypes, excluded_names,
            check_prefix + "_non_abi_absence",
            tolerated_exposed_names=tolerated_legacy_names), progress)

        if progress is not None:
            progress.start(check_prefix + "_constant_semantics")
        _append_check(checks, _cross_header_constant_semantics_check(
            manifest, header, implementation), progress)

        if progress is not None:
            progress.start(check_prefix + "_typedef_semantics")
        _append_check(checks, _cross_header_typedef_semantics_check(
            manifest, header, implementation), progress)

    if len(headers) == 2:
        if progress is not None:
            progress.start("cross_header_feature_availability")
        _append_check(checks, _cross_header_feature_set_check(
            headers, manifest, expected_signatures), progress)

    return checks


def _cross_wrapper_command(details, implementation):
    """Return wrapper query commands for one cross implementation."""
    if implementation == "open_mpi":
        return (
            ("compile", [details["compile_wrapper"], "--showme:compile"]),
            ("link", [details["compile_wrapper"], "--showme:link"]),
        )
    return (("show", [details["compile_wrapper"], "-show"]),)


def _cross_wrapper_intent_check(tools, dirs, implementation, details):
    """Verify wrapper ABI compile/link intent using parsed shell words."""
    check_name = "cross_wrapper_" + implementation + "_intent"
    commands = _cross_wrapper_command(details, implementation)
    outputs = {}
    words = []
    for label, command in commands:
        result = _command_result(
            check_name + "_" + label,
            command,
            dirs["base"],
            os.environ.copy(),
            dirs["logs"] / (check_name + "_" + label + ".json"))
        outputs[label] = result["log"]
        if result["returncode"] != 0:
            return _fail(
                check_name,
                "cross ABI wrapper query failed",
                implementation=implementation,
                command=result["command"],
                returncode=result["returncode"],
                log=result["log"])
        words.extend(shlex.split(result["stdout"]))

    words.extend(_cross_compile_overrides(tools, implementation))
    include_dirs = _flag_dirs_from_words(words, "-I")
    library_dirs = _flag_dirs_from_words(words, "-L")
    header = _cross_compile_header(tools, implementation)
    missing = []
    if header is None:
        missing.append("standard_abi_header")
    elif not any(Path(directory).resolve() == header.parent.resolve()
                 for directory in include_dirs):
        missing.append("standard_abi_include_path")
    if not _words_link_library(words, "mpi_abi"):
        missing.append("libmpi_abi_link")
    if implementation == "mpich":
        if not _words_define_macro(words, "MPI_ABI"):
            missing.append("MPI_ABI_define")

    if missing:
        return _fail(
            check_name,
            "cross ABI wrapper does not advertise required ABI intent",
            implementation=implementation,
            missing=missing,
            include_dirs=include_dirs,
            library_dirs=library_dirs,
            header=str(header) if header else None,
            logs=outputs)
    return _pass(
        check_name,
        implementation=implementation,
        include_dirs=include_dirs,
        library_dirs=library_dirs,
        header=str(header),
        logs=outputs)


def _cross_linkage_diagnostic_check():
    """Report whether cross executable linkage diagnostics are available."""
    system = platform.system()
    check_name = "cross_linkage_diagnostics_available"
    if system == "Linux":
        tool = shutil.which("ldd")
        if tool is None:
            return _skip(
                check_name,
                SKIP_LINKAGE_INSPECTION_UNAVAILABLE,
                platform=system,
                tool="ldd")
        return _pass(check_name, platform=system, tool=tool)
    if system == "Darwin":
        otool = shutil.which("otool")
        install_name_tool = shutil.which("install_name_tool")
        if otool is None or install_name_tool is None:
            return _skip(
                check_name,
                SKIP_LINKAGE_INSPECTION_UNAVAILABLE,
                platform=system,
                otool=otool,
                install_name_tool=install_name_tool)
        return _pass(
            check_name,
            platform=system,
            otool=otool,
            install_name_tool=install_name_tool)
    return _skip(
        check_name,
        SKIP_LINKAGE_INSPECTION_UNAVAILABLE,
        platform=system)


def _linkage_command(executable):
    """Return the platform-specific command for linkage inspection.

    Linkage inspection is a diagnostic check, not a portability
    requirement.  Platforms without readelf/otool return None and are
    reported as SKIP by the caller instead of making the ABI suite fail
    just because the inspection tool is unavailable.
    """
    system = platform.system()
    if system == "Darwin":
        tool = shutil.which("otool")
        if tool:
            return [tool, "-L", str(executable)]
    if system == "Linux":
        tool = shutil.which("readelf")
        if tool:
            return [tool, "-d", str(executable)]
    return None


def _verify_executable_libmpi_abi(executable, dirs, env, name):
    """Verify an executable is actually linked against libmpi_abi.

    The wrapper flag checks prove mpicc_abi advertises ABI linkage, but
    the executable inspection catches cases where overrides, stale
    paths, or wrapper bugs still produce a binary not linked to
    libmpi_abi.  Missing inspection tools are SKIP; a runnable tool that
    shows the wrong linkage is FAIL.
    """
    command = _linkage_command(executable)
    if command is None:
        return {
            "result": "SKIP",
            "skip_reason": SKIP_LINKAGE_INSPECTION_UNAVAILABLE,
            "command": None,
            "returncode": 127,
            "log": None,
        }

    result = _command_result(
        "linkage_" + name,
        command,
        dirs["base"],
        env,
        dirs["logs"] / ("linkage_" + name + ".json"))
    output = result["stdout"] + result["stderr"]
    if result["returncode"] != 0:
        result["result"] = "FAIL"
        result["message"] = "executable linkage inspection failed"
    elif re.search(r"(?<![A-Za-z0-9_])libmpi_abi(?:[.][A-Za-z0-9_.-]+)?",
                   output) is None:
        result["result"] = "FAIL"
        result["message"] = "executable is not linked against libmpi_abi"
    else:
        result["result"] = "PASS"
        result["message"] = None
    return result


def _cross_runtime_library_target(library_dir, library):
    """Return a concrete runtime ABI library path for a run side.

    The exact ABI shared-library suffix is platform and libtool
    dependent.  For Darwin rewrite operations we need one concrete file
    name for install_name_tool, but we must not assume a specific ABI
    library version number such as .0.
    """
    if not library_dir:
        return None
    directory = Path(library_dir)
    if not directory.exists():
        return None
    candidates = _abi_library_candidates(
        directory, library, include_static=False)
    if candidates:
        return str(candidates[0])
    return None


def _abi_library_candidates(directory, library, include_static=True):
    """Return matching ABI library files in a stable preference order."""
    candidates = [
        candidate for candidate in directory.iterdir()
        if _abi_library_basename_matches(
            candidate.name, library, include_static=include_static)
    ]
    return sorted(candidates, key=_abi_library_preference_key)


def _abi_library_preference_key(candidate):
    """Prefer dynamic ABI libraries before static archives."""
    name = candidate.name
    if ".dylib" in name:
        kind = 0
    elif ".so" in name:
        kind = 1
    elif name.endswith(".a"):
        kind = 2
    else:
        kind = 3
    return (kind, name)


def _abi_library_basename_matches(basename, library, include_static=True):
    """Return True when a basename is an ABI library for library."""
    prefix = "lib" + library
    patterns = [
        r"^{0}(?:[.][A-Za-z0-9_+-]+)*[.]dylib(?:[.].+)?$",
        r"^{0}[.]so(?:[.].+)?$",
    ]
    if include_static:
        patterns.append(r"^{0}[.]a$")
    return any(re.match(pattern.format(re.escape(prefix)), basename)
               for pattern in patterns)


def _path_is_run_side_abi_library(path, library_dir, library):
    """Return True when path names the selected run-side ABI library.

    Linux cross validation should prove that the dynamic loader resolved
    the ABI library from the run-side library directory.  It should not
    encode a particular libtool or distro shared-library version suffix.
    """
    if not path or not library_dir:
        return False
    resolved_path = Path(path).resolve()
    resolved_dir = Path(library_dir).resolve()
    return (resolved_path.parent == resolved_dir and
            _abi_library_basename_matches(
                resolved_path.name, library, include_static=False))


def _cross_verify_or_rewrite_abi_libraries(executable, dirs, env, name,
                                           runtime_library_path):
    """Verify or rewrite ABI library resolution to the selected run side.

    DYLD_LIBRARY_PATH cannot override absolute install names embedded by
    local Open MPI and MPICH builds.  Cross-direction execution therefore
    has to edit the generated test executable after compile and before
    launch.

    Important: the MPI ABI library is libmpi_abi.  Do not synthesize,
    shim, retarget, inspect, or require any other PMPI-specific library
    here.  MPI-5.0 section 21.2.1 says ABI application binaries must not
    require more than mpi_abi as the sole direct MPI ABI dependency, and
    section 16.2.1 still requires PMPI alternate entry points.  PMPI ABI
    symbols are therefore tested as symbols and calls through libmpi_abi,
    not through a separate library.  See Open MPI issue #13955.

    Linux does not need an equivalent rewrite, but it still needs a real
    cross-linkage check.  LD_LIBRARY_PATH can be defeated by RPATH/RUNPATH
    or other loader state, so use ldd to verify that retained ABI
    libraries resolve from the selected run-side directory before the
    probe is launched.
    """
    system = platform.system()
    if system == "Linux":
        return _cross_verify_linux_abi_libraries(
            executable, dirs, env, name, runtime_library_path)
    if system != "Darwin":
        return _skip("cross_runtime_linkage_" + name,
                     SKIP_LINKAGE_INSPECTION_UNAVAILABLE,
                     platform=system,
                     rewritten=False)

    # WARNING: macOS-specific test-harness workaround.
    #
    # This is the one known cross-runner compatibility adjustment that
    # is allowed for now.  Do not generalize it into "creative" ABI
    # compatibility behavior.  The intended MPI Forum ABI model is that
    # selecting the run-side library directory should be enough.  Local
    # Open MPI and MPICH macOS installs currently embed absolute
    # libmpi_abi install names, and dyld will keep using those absolute
    # compile-side paths even when DYLD_LIBRARY_PATH points at the run
    # side.  For temporary generated test executables only, rewrite
    # those absolute libmpi_abi load commands so the test actually
    # exercises the selected run-side ABI runtime.
    #
    # We have explicitly agreed to leave this exception in place for now.
    # Track removing it, or replacing it with normal @rpath behavior, in:
    #   https://github.com/open-mpi/ompi/issues/13956
    #
    otool = shutil.which("otool")
    install_name_tool = shutil.which("install_name_tool")
    if otool is None or install_name_tool is None:
        return _fail(
            "cross_runtime_linkage_" + name,
            "macOS cross runtime linkage rewrite tools are unavailable",
            platform=platform.system(),
            otool=otool,
            install_name_tool=install_name_tool)

    mpi_target = _cross_runtime_library_target(
        runtime_library_path, "mpi_abi")
    if mpi_target is None:
        return _fail(
            "cross_runtime_linkage_" + name,
            "run-side libmpi_abi is unavailable",
            runtime_library_path=runtime_library_path)

    inspect = _command_result(
        "otool_cross_" + name,
        [otool, "-L", str(executable)],
        dirs["base"],
        env,
        dirs["logs"] / ("otool_cross_" + name + ".json"))
    if inspect["returncode"] != 0:
        return _fail(
            "cross_runtime_linkage_" + name,
            "failed to inspect cross executable linkage",
            command=inspect["command"],
            returncode=inspect["returncode"],
            log=inspect["log"])

    changes = []
    saw_mpi_abi = False
    for line in inspect["stdout"].splitlines()[1:]:
        dependency = _darwin_otool_install_name(line)
        basename = Path(dependency).name
        target = None
        if basename.startswith("libmpi_abi."):
            saw_mpi_abi = True
            target = mpi_target
        if target is not None and dependency != target:
            changes.append((dependency, target))

    if not saw_mpi_abi:
        return _fail(
            "cross_runtime_linkage_" + name,
            "cross executable does not retain libmpi_abi",
            runtime_library_path=runtime_library_path,
            inspect_log=inspect["log"])

    change_logs = []
    for index, (old, new) in enumerate(changes):
        result = _command_result(
            "install_name_tool_cross_{0}_{1}".format(name, index),
            [install_name_tool, "-change", old, new, str(executable)],
            dirs["base"],
            env,
            dirs["logs"] / (
                "install_name_tool_cross_{0}_{1}.json".format(name, index)))
        change_logs.append(result["log"])
        if result["returncode"] != 0:
            return _fail(
                "cross_runtime_linkage_" + name,
                "failed to rewrite cross executable linkage",
                old=old,
                new=new,
                command=result["command"],
                returncode=result["returncode"],
                inspect_log=inspect["log"],
                change_logs=change_logs)

    return _pass(
        "cross_runtime_linkage_" + name,
        platform=platform.system(),
        rewritten=bool(changes),
        changes=[{"old": old, "new": new} for old, new in changes],
        inspect_log=inspect["log"],
        change_logs=change_logs)


def _cross_verify_linux_abi_libraries(executable, dirs, env, name,
                                      runtime_library_path):
    """Verify Linux cross probes resolve retained ABI libs from the run side."""
    ldd = shutil.which("ldd")
    if ldd is None:
        return _skip(
            "cross_runtime_linkage_" + name,
            SKIP_LINKAGE_INSPECTION_UNAVAILABLE,
            platform=platform.system(),
            tool="ldd")

    if _cross_runtime_library_target(runtime_library_path, "mpi_abi") is None:
        return _fail(
            "cross_runtime_linkage_" + name,
            "run-side libmpi_abi is unavailable",
            runtime_library_path=runtime_library_path)

    inspect = _command_result(
        "ldd_cross_" + name,
        [ldd, str(executable)],
        dirs["base"],
        env,
        dirs["logs"] / ("ldd_cross_" + name + ".json"))
    if inspect["returncode"] != 0:
        return _fail(
            "cross_runtime_linkage_" + name,
            "failed to inspect cross executable linkage",
            command=inspect["command"],
            returncode=inspect["returncode"],
            log=inspect["log"])

    output = inspect["stdout"]
    resolved = _linux_resolved_library(output, "libmpi_abi")
    if resolved is None:
        return _fail(
            "cross_runtime_linkage_" + name,
            "cross executable does not resolve libmpi_abi",
            runtime_library_path=runtime_library_path,
            inspect_log=inspect["log"])
    if not _path_is_run_side_abi_library(
            resolved, runtime_library_path, "mpi_abi"):
        return _fail(
            "cross_runtime_linkage_" + name,
            "cross executable resolves libmpi_abi from the wrong directory",
            resolved=resolved,
            runtime_library_path=runtime_library_path,
            inspect_log=inspect["log"])

    return _pass(
        "cross_runtime_linkage_" + name,
        platform=platform.system(),
        rewritten=False,
        resolved=resolved,
        runtime_library_path=runtime_library_path,
        inspect_log=inspect["log"])


def _darwin_otool_install_name(line):
    """Extract an install name from one otool -L dependency line.

    otool prints dependency lines as:

        <install name> (compatibility version ..., current version ...)

    The install name itself may contain spaces, so splitting on the
    first whitespace would silently skip rewrites for perfectly valid
    installation prefixes.
    """
    match = re.match(r"^\s*(.*?)\s+\(compatibility version\b", line)
    if match:
        return match.group(1)
    return line.strip()


def _linux_library_line(output, library):
    """Return the ldd match for a library name, if present."""
    pattern = re.compile(
        r"^\s*{0}(?:[.][^\s]*)?\s+=>\s+(.*?)"
        r"(?:\s+\(0x[0-9A-Fa-f]+\)|\s*$)".format(re.escape(library)))
    for line in output.splitlines():
        match = pattern.search(line)
        if match:
            return match
    return None


def _linux_resolved_library(output, library):
    """Return the resolved path for a library from ldd output."""
    match = _linux_library_line(output, library)
    if match:
        value = match.group(1)
        return None if value == "not found" else value
    return None


def _showme_words(mpicc_abi, option, dirs, env, name):
    """Run an mpicc_abi --showme option and shell-split its output."""
    result = _command_result(
        name,
        [mpicc_abi, option],
        dirs["base"],
        env,
        dirs["logs"] / (name + ".json"))
    if result["returncode"] != 0:
        return result, []
    return result, shlex.split(result["stdout"])


def _installed_standard_abi_header(tools, dirs, env):
    """Find the installed standard ABI mpi.h used by mpicc_abi.

    The MPI_H_ABI marker is required so an include path that happens to
    contain another mpi.h cannot satisfy the check.  This matters because
    installed probes intentionally compile outside the build tree.
    """
    include_override = tools["paths"]["include"]
    candidates = []
    if include_override:
        candidates.append(Path(include_override) / "mpi.h")

    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    _, incdirs = _showme_words(
        mpicc_abi, "--showme:incdirs", dirs, env, "mpicc_abi_showme_incdirs")
    for incdir in incdirs:
        candidates.append(Path(incdir) / "mpi.h")

    for candidate in candidates:
        if not candidate.exists():
            continue
        text = _read_text(candidate)
        if re.search(r"^\s*#define\s+MPI_H_ABI\b", text, re.MULTILINE):
            return candidate
    return None


def _parse_c_header_typedef_names(header):
    """Parse public MPI typedef names from a generated ABI header."""
    text = _remove_c_comments(_read_text(header))
    names = set()
    simple_re = re.compile(r"\btypedef\b[^;]*?\b(MPI_[A-Za-z0-9_]+)\s*;")
    function_re = re.compile(
        r"\btypedef\b[^;]*?\(\s*(MPI_[A-Za-z0-9_]+)\s*\)\s*\(")
    struct_alias_re = re.compile(r"(?m)^\s*}\s*(MPI_[A-Za-z0-9_]+)\s*;")
    names.update(simple_re.findall(text))
    names.update(function_re.findall(text))
    names.update(struct_alias_re.findall(text))
    return names


def _parse_c_header_deprecated_functions(srcdir):
    """Read the generator's authoritative deprecated C function list."""
    c_header = srcdir / "ompi" / "mpi" / "bindings" / "c_header.py"
    tree = ast.parse(_read_text(c_header), filename=str(c_header))
    for node in tree.body:
        if not isinstance(node, ast.Assign):
            continue
        if not any(isinstance(target, ast.Name)
                   and target.id == "DEPRECATED_FUNCTIONS"
                   for target in node.targets):
            continue
        value = ast.literal_eval(node.value)
        return set(value)
    raise RuntimeError("DEPRECATED_FUNCTIONS not found in {0}".format(
        c_header))


def _parse_c_signature(signature):
    """Parse one normalized MPI C signature into comparable fields."""
    signature = _normalize_c_signature_text(signature)
    match = re.match(
        r"^(?P<return_type>.+?)\s+(?P<name>(?:P)?MPI_\w+)\("
        r"(?P<args>.*)\)$",
        signature)
    if match is None:
        return None
    return {
        "name": match.group("name"),
        "return_type": _normalize_c_signature_text(
            match.group("return_type")),
        "args": _normalize_c_signature_text(match.group("args")),
        "signature": signature,
    }


def _split_c_parameter_list(args):
    """Split a simple C parameter list into individual parameters."""
    args = _normalize_c_signature_text(args)
    if args == "void":
        return []
    return [item.strip() for item in args.split(",")]


def _normalize_c_parameter_type(parameter):
    """Return a parameter type spelling without its non-ABI name.

    Cross-implementation header checks must compare ABI-relevant C
    types, not parameter names.  They also need to treat equivalent
    prototype spellings such as int values[] and int *values as the
    same pointer type.
    """
    parameter = _normalize_c_signature_text(parameter)
    parameter = re.sub(
        r"\s+[A-Za-z_][A-Za-z0-9_]*\s*\[\]$", "*", parameter)
    parameter = re.sub(
        r"\s*\*\s*[A-Za-z_][A-Za-z0-9_]*$", "*", parameter)
    parameter = re.sub(
        r"\s+[A-Za-z_][A-Za-z0-9_]*$", "", parameter)
    return _normalize_c_signature_text(parameter)


def _normalized_c_parameter_types(args):
    """Return ABI-relevant parameter types for a C prototype."""
    return tuple(
        _normalize_c_parameter_type(parameter)
        for parameter in _split_c_parameter_list(args)
    )


def _c_parameter_normalization_unit_checks():
    """Validate C parameter normalization used by header comparison.

    Cross-header comparison intentionally ignores parameter names while
    preserving ABI-relevant type spellings.  Array and pointer spelling
    differences are particularly important because generated headers and
    vendor headers may spell the same ABI contract differently.
    """
    check_name = "fast_c_parameter_normalization_unit_checks"
    cases = {
        "int count": "int",
        "MPI_Comm comm": "MPI_Comm",
        "void *attribute_val": "void*",
        "int array_of_blocklengths[]": "int*",
        "const MPI_Aint array_of_displacements[]": "const MPI_Aint*",
        "MPI_Datatype *newtype": "MPI_Datatype*",
    }
    mismatches = {
        parameter: {
            "expected": expected,
            "actual": _normalize_c_parameter_type(parameter),
        }
        for parameter, expected in cases.items()
        if _normalize_c_parameter_type(parameter) != expected
    }
    if mismatches:
        return _fail(
            check_name,
            "C parameter normalization changed unexpectedly",
            check="parameter_normalization",
            mismatches=mismatches)

    args = (
        "MPI_Comm comm, int array_of_ranks[], "
        "MPI_Aint *array_of_displacements"
    )
    normalized = _normalized_c_parameter_types(args)
    expected = ("MPI_Comm", "int*", "MPI_Aint*")
    if normalized != expected:
        return _fail(
            check_name,
            "C parameter-list normalization changed unexpectedly",
            check="parameter_list_normalization",
            expected=expected,
            actual=normalized)
    return _pass(check_name)


def _normalize_c_typedef_reference(type_name):
    """Return the typedef name referenced by a metadata C type string."""
    type_name = _normalize_c_parameter_type(type_name)
    return type_name.rstrip("*").strip()


def _standard_abi_expected_signatures(srcdir):
    """Build expected C/PMPI signatures from pympistandard metadata.

    Comparing against pympistandard avoids a circular test where the
    installed header is parsed and then used as its own authority.
    Deprecated C APIs and Fortran-only entry points are excluded by the
    same generator-side rules used to build the standard ABI header.
    """
    sys.path.insert(
        0, str((srcdir / "3rd-party" / "pympistandard" / "src").resolve()))
    import pympistandard as std

    std.use_api_version()
    deprecated = _parse_c_header_deprecated_functions(srcdir)
    expected = {}
    excluded = set()

    def add_signature(signature):
        signature = str(signature).replace(r"\ldots", "...")
        signature = signature.replace("@MPI_COUNT@", "MPI_Count")
        signature = signature.replace("@MPI_AINT@", "MPI_Aint")
        signature = signature.replace("@MPI_OFFSET@", "MPI_Offset")
        signature = signature.replace("char argv[]", "char *argv[]")
        parsed = _parse_c_signature(signature)
        if parsed is None:
            raise RuntimeError("could not parse expected C signature: {0}".
                               format(signature))
        name = parsed["name"]
        if any(function in signature for function in deprecated):
            excluded.add(name)
            return
        if "MPI_Fint" in signature or "MPI_F08_status" in signature:
            excluded.add(name)
            return
        expected[name] = parsed

    for proc in std.all_iso_c_procedures():
        add_signature(proc.express.iso_c)
        if proc.has_embiggenment():
            add_signature(proc.express.embiggen.iso_c)

    for proc in std.all_iso_c_procedures():
        add_signature(proc.express.profile.iso_c)
        if proc.has_embiggenment():
            binding = str(proc.express.embiggen.iso_c).split()
            add_signature("{0} P{1};".format(binding[0],
                                             " ".join(binding[1:])))

    return expected, excluded


def _probe_variable_name(name):
    return "probe_" + re.sub(r"[^A-Za-z0-9_]", "_", name)


def _header_probe_source(prototypes):
    """Generate a C source that forces references to parsed prototypes.

    Parsing a prototype is not enough; this source assigns every parsed
    MPI/PMPI symbol to a function pointer so the compiler and linker must
    accept the declarations and resolve the ABI entry points.
    """
    lines = [
        "#include \"mpi.h\"",
        "",
        "#ifndef MPI_H_ABI",
        "#error \"standard ABI mpi.h was not used\"",
        "#endif",
        "",
    ]
    for name in sorted(prototypes):
        proto = prototypes[name]
        variable = _probe_variable_name(name)
        lines.append("static {0} (*{1})({2}) = {3};".format(
            proto["return_type"], variable, proto["args"], name))

    lines.extend([
        "",
        "int main(void)",
        "{",
    ])
    for name in sorted(prototypes):
        variable = _probe_variable_name(name)
        lines.extend([
            "    if (0 == {0}) {{".format(variable),
            "        return 1;",
            "    }",
        ])
    lines.extend([
        "    return 0;",
        "}",
        "",
    ])
    return "\n".join(lines)


def _metadata_c_api_header_check(manifest, mpi_prototypes, excluded_names):
    """Ensure all implemented C ABI APIs are present in installed mpi.h."""
    expected = []
    missing = []
    non_abi_absent = []
    for entry in manifest["apis"]:
        if entry["classification"] != CLASS_IMPLEMENTED:
            continue
        if not entry["languages"]["c"]:
            continue
        name = entry["name"]
        if name in excluded_names:
            non_abi_absent.append(name)
            continue
        expected.append(name)
        if name not in mpi_prototypes:
            missing.append(name)

    if missing:
        return _fail(
            "installed_c_header_metadata_apis",
            "implemented C ABI metadata entries are missing from mpi.h",
            checked=len(expected),
            missing=missing[:20],
            missing_count=len(missing),
            non_abi_absent_count=len(non_abi_absent))

    return _pass(
        "installed_c_header_metadata_apis",
        checked=len(expected),
        non_abi_absent_count=len(non_abi_absent))


def _signature_comparison_check(
        prototypes, expected_signatures,
        check_name="installed_c_header_signature_semantics",
        compare_parameter_names=True,
        tolerated_extra_names=None):
    """Compare installed header prototypes against generated signatures."""
    if tolerated_extra_names is None:
        tolerated_extra_names = set()
    missing = []
    mismatches = []
    for name, expected in sorted(expected_signatures.items()):
        actual = prototypes.get(name)
        if actual is None:
            missing.append(name)
            continue
        if compare_parameter_names:
            args_match = actual["args"] == expected["args"]
        else:
            args_match = (
                _normalized_c_parameter_types(actual["args"]) ==
                _normalized_c_parameter_types(expected["args"])
            )
        if actual["return_type"] != expected["return_type"] or not args_match:
            mismatches.append({
                "name": name,
                "expected": expected["signature"],
                "actual": actual["signature"],
            })

    extra = sorted(
        name for name in prototypes
        if name not in expected_signatures and
        (name.startswith("MPI_") or name.startswith("PMPI_"))
        and name not in tolerated_extra_names
    )
    tolerated_extra = sorted(
        name for name in prototypes
        if name not in expected_signatures and name in tolerated_extra_names
    )

    if missing or mismatches or extra:
        return _fail(
            check_name,
            "standard ABI header signatures differ from MPI C signatures",
            checked=len(expected_signatures),
            missing=missing[:20],
            missing_count=len(missing),
            mismatches=mismatches[:20],
            mismatch_count=len(mismatches),
            extra=extra[:20],
            extra_count=len(extra),
            tolerated_extra=tolerated_extra,
            tolerated_extra_count=len(tolerated_extra))

    return _pass(
        check_name,
        checked=len(expected_signatures),
        tolerated_extra=tolerated_extra,
        tolerated_extra_count=len(tolerated_extra))


def _non_abi_absence_check(
        prototypes, excluded_names,
        check_name="installed_c_header_non_abi_absence",
        tolerated_exposed_names=None):
    """Ensure APIs excluded from the standard ABI are not exposed."""
    if tolerated_exposed_names is None:
        tolerated_exposed_names = set()
    exposed = [
        name for name in sorted(prototypes)
        if (name.startswith("MPI_") or name.startswith("PMPI_"))
        and name in excluded_names and name not in tolerated_exposed_names
    ]
    tolerated_exposed = [
        name for name in sorted(prototypes)
        if (name.startswith("MPI_") or name.startswith("PMPI_"))
        and name in excluded_names and name in tolerated_exposed_names
    ]
    if exposed:
        return _fail(
            check_name,
            "non-ABI C APIs are exposed by the standard ABI header",
            exposed=exposed[:20],
            exposed_count=len(exposed),
            tolerated_exposed=tolerated_exposed,
            tolerated_exposed_count=len(tolerated_exposed))
    return _pass(
        check_name,
        checked=len(excluded_names),
        tolerated_exposed=tolerated_exposed,
        tolerated_exposed_count=len(tolerated_exposed))


def _prototype_pair_check(prototypes, excluded_names):
    """Ensure each MPI prototype has its matching PMPI prototype."""
    mpi_names = sorted(
        name for name in prototypes
        if name.startswith("MPI_") and name not in excluded_names
    )
    pmpi_names = sorted(
        name for name in prototypes if name.startswith("PMPI_"))
    missing_pmpi = []
    for name in mpi_names:
        pmpi_name = "P" + name
        if pmpi_name not in prototypes:
            missing_pmpi.append(pmpi_name)

    if missing_pmpi:
        return _fail(
            "installed_c_header_api_prototypes",
            "standard ABI header is missing PMPI prototypes",
            mpi_count=len(mpi_names),
            pmpi_count=len(pmpi_names),
            missing_pmpi=missing_pmpi[:20],
            missing_pmpi_count=len(missing_pmpi))

    return _pass("installed_c_header_api_prototypes",
                 mpi_count=len(mpi_names),
                 pmpi_count=len(pmpi_names))


def _installed_libmpi_abi_path(tools, dirs, env):
    """Locate installed libmpi_abi using overrides and wrapper metadata."""
    library_override = tools["paths"]["library"]
    libdirs = []
    if library_override:
        libdirs.append(library_override)

    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    _, showme_libdirs = _showme_words(
        mpicc_abi, "--showme:libdirs", dirs, env, "mpicc_abi_showme_libdirs")
    libdirs.extend(showme_libdirs)

    for libdir in libdirs:
        directory = Path(libdir)
        if not directory.exists():
            continue
        candidates = _abi_library_candidates(directory, "mpi_abi")
        if candidates:
            return candidates[0]
    return None


def _defined_nm_symbols(output):
    """Extract defined symbol names from nm output.

    Undefined symbols, including weak undefined symbols, are deliberately
    excluded.  Counting a referenced-but-not-defined MPI symbol as an
    export would make the libmpi_abi symbol check pass for a missing ABI
    entry point.
    """
    symbols = set()
    for line in output.splitlines():
        parts = line.split()
        if len(parts) < 2:
            continue
        symbol = parts[-1]
        symbol_type = parts[-2] if len(parts) >= 3 else parts[0]
        if symbol_type in ("U", "w", "v"):
            continue
        if symbol.startswith("_"):
            symbol = symbol[1:]
        symbols.add(symbol)
    return symbols


def _symbol_table_check(prototypes, excluded_names, tools, dirs, env):
    """Optionally verify libmpi_abi exports all expected ABI symbols."""
    nm = shutil.which("nm")
    library = _installed_libmpi_abi_path(tools, dirs, env)
    if nm is None or library is None:
        return _skip(
            "installed_libmpi_abi_symbols",
            SKIP_SYMBOL_DIAGNOSTICS_UNAVAILABLE,
            nm=nm,
            library=str(library) if library else None)

    result = _command_result(
        "nm_libmpi_abi",
        [nm, "-g", str(library)],
        dirs["base"],
        env,
        dirs["logs"] / "nm_libmpi_abi.json")
    if result["returncode"] != 0:
        return _fail(
            "installed_libmpi_abi_symbols",
            "nm failed while inspecting libmpi_abi",
            command=result["command"],
            returncode=result["returncode"],
            log=result["log"],
            library=str(library))

    output = result["stdout"] + result["stderr"]
    defined_symbols = _defined_nm_symbols(output)
    expected = [
        name for name in sorted(prototypes)
        if (name.startswith("MPI_") or name.startswith("PMPI_"))
        and name not in excluded_names
    ]
    missing = [
        name for name in expected
        if name not in defined_symbols
    ]
    if missing:
        return _fail(
            "installed_libmpi_abi_symbols",
            "libmpi_abi is missing defined standard ABI symbols",
            library=str(library),
            checked=len(expected),
            missing=missing[:20],
            missing_count=len(missing),
            log=result["log"])

    return _pass(
        "installed_libmpi_abi_symbols",
        library=str(library),
        checked=len(expected),
        log=result["log"])


def _installed_c_header_symbol_checks(manifest, tools, dirs, progress=None):
    """Run installed header, signature, compile/link, and symbol checks.

    This combines several independent views of the installed C ABI:
    metadata coverage, PMPI pairing, semantic signatures, absence of
    non-ABI APIs, compile/link reachability, and optional library symbol
    diagnostics.  Keeping them separate makes failures actionable.
    """
    checks = []
    env = _installed_test_env(tools)
    if progress is not None:
        progress.start("installed_c_header_api_prototypes")
    header = _installed_standard_abi_header(tools, dirs, env)
    if header is None:
        check = _skip("installed_c_header_api_prototypes",
                      SKIP_HEADER_UNAVAILABLE)
        if progress is not None:
            progress.check(check)
        return [check]

    prototypes = _parse_c_header_prototypes(header)
    if len(prototypes) < MIN_EXPECTED_C_HEADER_PROTOTYPES:
        check = _fail(
            "installed_c_header_api_prototypes",
            "standard ABI header prototype parser found too few entries",
            header=str(header),
            prototype_count=len(prototypes),
            minimum_expected=MIN_EXPECTED_C_HEADER_PROTOTYPES)
        if progress is not None:
            progress.check(check)
        return [check]

    expected_signatures, excluded_names = _standard_abi_expected_signatures(
        Path(manifest["metadata"]["api_path"]).parents[1])
    mpi_prototypes = {
        name: proto for name, proto in prototypes.items()
        if name.startswith("MPI_")
    }
    _append_check(
        checks, _prototype_pair_check(prototypes, excluded_names), progress)

    if progress is not None:
        progress.start("installed_c_header_metadata_apis")
    _append_check(checks, _metadata_c_api_header_check(
        manifest, mpi_prototypes, excluded_names), progress)

    if progress is not None:
        progress.start("installed_c_header_signature_semantics")
    _append_check(checks, _signature_comparison_check(
        prototypes, expected_signatures), progress)

    if progress is not None:
        progress.start("installed_c_header_non_abi_absence")
    _append_check(
        checks, _non_abi_absence_check(prototypes, excluded_names), progress)

    source = dirs["src"] / "c_header_api_prototypes.c"
    executable = dirs["bin"] / "c_header_api_prototypes"
    _write_text(source, _header_probe_source(prototypes))

    compile_command = (
        [tools["open_mpi"]["mpicc_abi"]] + _compile_overrides(tools) +
        [str(source), "-o", str(executable)]
    )
    if progress is not None:
        progress.start("installed_c_header_compile_link")
    compile_result = _command_result(
        "compile_c_header_api_prototypes",
        compile_command,
        dirs["base"],
        env,
        dirs["logs"] / "compile_c_header_api_prototypes.json")
    if compile_result["returncode"] != 0:
        _append_check(checks, _fail(
            "installed_c_header_compile_link",
            "standard ABI header prototype compile/link probe failed",
            source=str(source),
            executable=str(executable),
            command=compile_result["command"],
            returncode=compile_result["returncode"],
            log=compile_result["log"],
            prototype_count=len(prototypes)), progress)
        return checks

    linkage_result = _verify_executable_libmpi_abi(
        executable, dirs, env, "c_header_api_prototypes")
    if linkage_result["result"] == "SKIP":
        _append_check(checks, _skip(
            "installed_c_header_compile_link",
            linkage_result["skip_reason"],
            source=str(source),
            executable=str(executable),
            compile_log=compile_result["log"],
            linkage_log=linkage_result["log"],
            prototype_count=len(prototypes)), progress)
    elif linkage_result["result"] != "PASS":
        _append_check(checks, _fail(
            "installed_c_header_compile_link",
            linkage_result["message"],
            source=str(source),
            executable=str(executable),
            command=linkage_result["command"],
            returncode=linkage_result["returncode"],
            compile_log=compile_result["log"],
            linkage_log=linkage_result["log"],
            prototype_count=len(prototypes)), progress)
    else:
        _append_check(checks, _pass(
            "installed_c_header_compile_link",
            source=str(source),
            executable=str(executable),
            header=str(header),
            prototype_count=len(prototypes),
            compile_log=compile_result["log"],
            linkage_log=linkage_result["log"]), progress)

    if progress is not None:
        progress.start("installed_libmpi_abi_symbols")
    _append_check(checks, _symbol_table_check(prototypes, excluded_names,
                                              tools, dirs, env), progress)
    return checks


def _installed_wrapper_checks(tools, dirs, progress=None):
    """Check mpicc_abi wrapper flags for ABI include and library linkage.

    These are coarse wrapper sanity checks.  They are intentionally not
    the only proof of correctness; later installed header and executable
    linkage checks verify that the advertised paths actually work.
    """
    checks = []
    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    env = _installed_test_env(tools)
    compile_log = dirs["logs"] / "mpicc_abi_showme_compile.json"
    link_log = dirs["logs"] / "mpicc_abi_showme_link.json"
    if progress is not None:
        progress.start("installed_standard_abi_wrapper_flags")
    compile_result = _command_result(
        "mpicc_abi_showme_compile",
        [mpicc_abi, "--showme:compile"],
        dirs["base"],
        env,
        compile_log)

    if compile_result["returncode"] != 0:
        _append_check(checks, _fail(
            "installed_standard_abi_wrapper_flags",
            "mpicc_abi --showme:compile failed",
            command=compile_result["command"],
            returncode=compile_result["returncode"],
            log=compile_result["log"]), progress)
    elif "standard_abi" not in compile_result["stdout"]:
        _append_check(checks, _fail(
            "installed_standard_abi_wrapper_flags",
            "mpicc_abi does not advertise standard ABI include path",
            command=compile_result["command"],
            stdout=compile_result["stdout"],
            log=compile_result["log"]), progress)
    else:
        _append_check(checks, _pass(
            "installed_standard_abi_wrapper_flags",
            command=compile_result["command"],
            stdout=compile_result["stdout"].strip(),
            log=compile_result["log"]), progress)

    if progress is not None:
        progress.start("installed_libmpi_abi_wrapper_flags")
    link_result = _command_result(
        "mpicc_abi_showme_link",
        [mpicc_abi, "--showme:link"],
        dirs["base"],
        env,
        link_log)
    if link_result["returncode"] != 0:
        _append_check(checks, _fail(
            "installed_libmpi_abi_wrapper_flags",
            "mpicc_abi --showme:link failed",
            command=link_result["command"],
            returncode=link_result["returncode"],
            log=link_result["log"]), progress)
    elif re.search(r"(?<![A-Za-z0-9_])-lmpi_abi(?![A-Za-z0-9_])",
                   link_result["stdout"]) is None:
        _append_check(checks, _fail(
            "installed_libmpi_abi_wrapper_flags",
            "mpicc_abi does not advertise libmpi_abi linkage",
            command=link_result["command"],
            stdout=link_result["stdout"],
            log=link_result["log"]), progress)
    else:
        _append_check(checks, _pass(
            "installed_libmpi_abi_wrapper_flags",
            command=link_result["command"],
            stdout=link_result["stdout"].strip(),
            log=link_result["log"]), progress)

    return checks


def _c_probe_source(srcdir, case, body, rank_count):
    """Render one installed C probe source from the shared template."""
    template = _read_text(srcdir / "test" / "mpi-abi" /
                          "templates" / "c_probe.c.in")
    body = body.replace("@EXPECTED_RANKS@", str(rank_count))
    prologue = _probe_prologue_text(srcdir, case).rstrip()
    # Keep checked-in *.cbody.in snippets at natural column-zero C
    # indentation.  The snippets are always inserted inside main(), so
    # the generated source owns the function-body indentation instead of
    # baking an extraction artifact into every snippet file.
    body = "\n".join(
        "    " + line if line else line
        for line in body.rstrip().splitlines())
    return (
        template
        .replace("@PROLOGUE@", prologue)
        .replace("@BODY@", body.rstrip())
    )


def _fortran_probe_source(srcdir, case):
    """Render one compile-only Fortran probe from the shared template."""
    template = _read_text(srcdir / "test" / "mpi-abi" /
                          "templates" / "fortran_probe.f90.in")
    body = case["body"].strip()
    return (
        template
        .replace("@USE_STATEMENT@", case["use_statement"])
        .replace("@BODY@", body)
    )


def _fortran_binding_skip(manifest, tools, language):
    """Return a stable skip reason for unavailable Fortran checks."""
    state = manifest["configuration"]["fortran"][language]
    if state["enabled"] is False:
        return SKIP_FORTRAN_BINDING_DISABLED
    if not _tool_available(tools["open_mpi"].get("mpifort")):
        return SKIP_FORTRAN_WRAPPER_UNAVAILABLE
    return None


def _fortran_check_api_names(checks):
    """Return API names covered by successful Fortran probe results.

    Static case metadata is useful for generating probes, but runtime
    coverage must come from checks that actually PASS.  This keeps a
    probe that compiles but exits through a stable SKIP from being
    reported as runtime coverage.
    """
    covered = {language: set() for language in FORTRAN_BINDING_LANGUAGES}
    for check in checks:
        language = check.get("language")
        if check.get("result") == "PASS" and language in covered:
            covered[language].update(check.get("api_names", ()))
    return covered


def _fortran_coverage_audit(manifest, tools, compile_checks, runtime_checks):
    """Report configured Fortran coverage populations for Phase 11.

    Phase 11 grows in layers.  The audit is intentionally grouped by
    binding because mpif.h/use mpi are regression coverage, while
    use mpi_f08 is the standard Fortran ABI-relevant layer.  Until the
    exhaustive generated Fortran probes exist, the audit reports pending
    APIs instead of treating them as hidden PASSes.
    """
    compile_covered = _fortran_check_api_names(compile_checks)
    runtime_covered = _fortran_check_api_names(runtime_checks)
    by_language = {}
    for language in FORTRAN_BINDING_LANGUAGES:
        state = manifest["configuration"]["fortran"][language]
        expressible = [
            entry for entry in manifest["apis"]
            if entry["languages"].get(language)
        ]
        implemented = [
            entry for entry in expressible
            if entry["classification"] == CLASS_IMPLEMENTED
        ]
        covered_names = compile_covered[language] | runtime_covered[language]
        covered_implemented = sorted(
            entry["name"] for entry in implemented
            if entry["name"] in covered_names
        )
        pending = sorted(
            entry["name"] for entry in implemented
            if entry["name"] not in covered_names
        )
        by_language[language] = {
            "configured": state,
            "skip_reason": _fortran_binding_skip(manifest, tools, language),
            "expressible_count": len(expressible),
            "implemented_count": len(implemented),
            "compile_probe_api_names": sorted(compile_covered[language]),
            "runtime_probe_api_names": sorted(runtime_covered[language]),
            "covered_implemented_count": len(covered_implemented),
            "pending_phase11b_count": len(pending),
            "pending_phase11b": pending[:20],
            "coverage_kind": (
                "standard_abi" if language == "use mpi_f08"
                else "regression"
            ),
        }

    return _pass(
        "installed_fortran_coverage_audit",
        enforcement="advisory_until_exhaustive_fortran_generation",
        languages=by_language,
        mpifort=tools["open_mpi"].get("mpifort"))


def _run_installed_fortran_compile_probes(srcdir, manifest, tools, dirs,
                                          progress=None):
    """Compile one installed Fortran probe per configured binding layer."""
    checks = []
    mpifort = tools["open_mpi"].get("mpifort")
    env = _installed_test_env(tools)
    compile_overrides = _compile_overrides(tools)

    for case in INSTALLED_FORTRAN_COMPILE_PROBES:
        name = case["name"]
        language = case["language"]
        check_name = "installed_" + name
        skip_reason = _fortran_binding_skip(manifest, tools, language)
        if skip_reason is not None:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _skip(
                check_name,
                skip_reason,
                phase="configure",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                mpifort=mpifort), progress)
            continue

        source = dirs["src"] / (name + ".f90")
        executable = dirs["bin"] / name
        _write_text(source, _fortran_probe_source(srcdir, case))
        compile_command = (
            [mpifort] + compile_overrides +
            [str(source), "-o", str(executable)]
        )
        if progress is not None:
            progress.start(check_name)
        compile_result = _command_result(
            "compile_" + name,
            compile_command,
            dirs["base"],
            env,
            dirs["logs"] / ("compile_" + name + ".json"))
        if compile_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "installed Fortran binding compile probe failed",
                phase="compile",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                source=str(source),
                executable=str(executable),
                command=compile_result["command"],
                returncode=compile_result["returncode"],
                log=compile_result["log"]), progress)
            continue

        _append_check(checks, _pass(
            check_name,
            language=language,
            configured=manifest["configuration"]["fortran"][language],
            source=str(source),
            executable=str(executable),
            api_names=list(case.get("api_names", ())),
            compile_command=compile_result["command"],
            compile_log=compile_result["log"]), progress)

    return checks


def _run_installed_fortran_runtime_probes(srcdir, manifest, tools, dirs,
                                          progress=None):
    """Compile and launch one installed Fortran runtime probe per case."""
    checks = []
    mpifort = tools["open_mpi"].get("mpifort")
    mpirun = tools["open_mpi"]["mpirun"]
    env = _installed_test_env(tools)
    launcher_args = _launcher_args(tools)
    compile_overrides = _compile_overrides(tools)

    for case in INSTALLED_FORTRAN_RUNTIME_PROBES:
        name = case["name"]
        language = case["language"]
        check_name = "installed_" + name
        skip_reason = _fortran_binding_skip(manifest, tools, language)
        if skip_reason is not None:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _skip(
                check_name,
                skip_reason,
                phase="configure",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                mpifort=mpifort), progress)
            continue

        rank_count = tools["rank_counts"]["np{0}".format(
            case["rank_count"])]
        source = dirs["src"] / (name + ".f90")
        executable = dirs["bin"] / name
        _write_text(source, _fortran_probe_source(srcdir, case))
        compile_command = (
            [mpifort] + compile_overrides +
            [str(source), "-o", str(executable)]
        )
        if progress is not None:
            progress.start(check_name)
        compile_result = _command_result(
            "compile_" + name,
            compile_command,
            dirs["base"],
            env,
            dirs["logs"] / ("compile_" + name + ".json"))
        if compile_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "installed Fortran runtime probe compile failed",
                phase="compile",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                source=str(source),
                executable=str(executable),
                command=compile_result["command"],
                returncode=compile_result["returncode"],
                log=compile_result["log"]), progress)
            continue

        run_command = (
            [mpirun] + launcher_args +
            ["-n", str(rank_count), str(executable)]
        )
        run_result = _command_result(
            "run_" + name,
            run_command,
            dirs["base"],
            env,
            dirs["logs"] / ("run_" + name + ".json"))
        if run_result["returncode"] != 0:
            if run_result["timed_out"]:
                _append_check(checks, _fail(
                    check_name,
                    "installed Fortran runtime probe timed out",
                    phase="run",
                    language=language,
                    configured=manifest["configuration"]["fortran"][
                        language],
                    source=str(source),
                    executable=str(executable),
                    rank_count=rank_count,
                    command=run_result["command"],
                    returncode=run_result["returncode"],
                    timeout=run_result["timeout"],
                    timed_out=run_result["timed_out"],
                    compile_log=compile_result["log"],
                    run_log=run_result["log"]), progress)
                continue
            skip_reason = case.get("skip_exit_codes", {}).get(
                run_result["returncode"])
            if skip_reason is not None:
                _append_check(checks, _skip(
                    check_name,
                    skip_reason,
                    phase="run",
                    language=language,
                    configured=manifest["configuration"]["fortran"][
                        language],
                    source=str(source),
                    executable=str(executable),
                    rank_count=rank_count,
                    command=run_result["command"],
                    returncode=run_result["returncode"],
                    compile_log=compile_result["log"],
                    run_log=run_result["log"]), progress)
                continue

            _append_check(checks, _fail(
                check_name,
                "installed Fortran runtime probe failed",
                phase="run",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                source=str(source),
                executable=str(executable),
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"]), progress)
            continue

        _append_check(checks, _pass(
            check_name,
            language=language,
            configured=manifest["configuration"]["fortran"][language],
            source=str(source),
            executable=str(executable),
            rank_count=rank_count,
            api_names=list(case.get("api_names", ())),
            compile_command=compile_result["command"],
            run_command=run_result["command"],
            compile_log=compile_result["log"],
            run_log=run_result["log"]), progress)

    return checks


def _fortran_optional_datatype_skip(manifest, tools, progress=None):
    """Record the still-deferred optional Fortran datatype work item.

    The C ABI converter tests already guard optional Fortran datatype
    declarations against the installed standard ABI header.  Exhaustive
    Fortran binding probes need generated source driven by the installed
    module declarations, which is intentionally left as a distinct open
    task so unavailable optional types become precise SKIPs instead of
    compile failures.
    """
    check_name = "installed_fortran_optional_datatype_generation"
    if progress is not None:
        progress.start(check_name)
    return _skip(
        check_name,
        SKIP_FORTRAN_OPTIONAL_DATATYPES_DEFERRED,
        phase="generation",
        mpifort=tools["open_mpi"].get("mpifort"),
        fortran=manifest["configuration"]["fortran"])


def _installed_fortran_checks(srcdir, manifest, tools, dirs, progress=None):
    """Run installed Fortran detection, compile, and runtime checks."""
    checks = []
    compile_checks = _run_installed_fortran_compile_probes(
        srcdir, manifest, tools, dirs, progress)
    checks.extend(compile_checks)
    runtime_checks = _run_installed_fortran_runtime_probes(
        srcdir, manifest, tools, dirs, progress)
    checks.extend(runtime_checks)
    if progress is not None:
        progress.start("installed_fortran_coverage_audit")
    _append_check(checks, _fortran_coverage_audit(
        manifest, tools, compile_checks, runtime_checks), progress)
    _append_check(checks, _fortran_optional_datatype_skip(
        manifest, tools, progress), progress)
    return checks


def _run_installed_c_probe_cases(srcdir, manifest, tools, dirs, header_names,
                                 cases, progress=None):
    """Compile, link-inspect, and launch installed C probe cases.

    Keep this loop shared by the Phase 1-8 ABI/converter probes and the
    Phase 9 runtime API probes.  The caller decides which generation
    preflights must pass before a case group runs, which prevents a
    Phase 9 table issue from suppressing older ABI coverage.
    """
    checks = []
    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    mpirun = tools["open_mpi"]["mpirun"]
    env = _installed_test_env(tools)
    launcher_args = _launcher_args(tools)
    compile_overrides = _compile_overrides(tools)

    for case in cases:
        name = case["name"]
        check_name = "installed_c_probe_" + name
        if (case.get("requires_fortran")
                and not _fortran_bindings_enabled(manifest)):
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _skip(
                check_name,
                SKIP_FORTRAN_BINDINGS_DISABLED,
                phase="configure"), progress)
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
                    feature=feature,
                    feature_state=feature_info), progress)
                continue
        rank_count = tools["rank_counts"]["np{0}".format(
            case["rank_count"])]
        source = dirs["src"] / (name + ".c")
        executable = dirs["bin"] / name
        try:
            body = _prepare_installed_c_probe_body(
                srcdir, case, manifest, header_names)
        except RuntimeError as exc:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe body is unavailable",
                phase="source",
                error=str(exc)), progress)
            continue
        try:
            probe_source = _c_probe_source(srcdir, case, body, rank_count)
        except RuntimeError as exc:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe prologue is unavailable",
                phase="source",
                error=str(exc)), progress)
            continue
        _write_text(source, probe_source)

        compile_command = (
            [mpicc_abi] + compile_overrides +
            [str(source), "-o", str(executable)]
        )
        if progress is not None:
            progress.start(check_name)
        compile_result = _command_result(
            "compile_" + name,
            compile_command,
            dirs["base"],
            env,
            dirs["logs"] / ("compile_" + name + ".json"))
        if compile_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe compile failed",
                phase="compile",
                source=str(source),
                executable=str(executable),
                command=compile_result["command"],
                returncode=compile_result["returncode"],
                log=compile_result["log"]), progress)
            continue

        linkage_result = _verify_executable_libmpi_abi(
            executable, dirs, env, name)
        if linkage_result["result"] == "SKIP":
            _append_check(checks, _skip(
                check_name,
                linkage_result["skip_reason"],
                phase="linkage",
                source=str(source),
                executable=str(executable),
                command=linkage_result["command"],
                returncode=linkage_result["returncode"],
                compile_log=compile_result["log"],
                linkage_log=linkage_result["log"]), progress)
            continue

        if linkage_result["result"] != "PASS":
            _append_check(checks, _fail(
                check_name,
                linkage_result["message"],
                phase="linkage",
                source=str(source),
                executable=str(executable),
                command=linkage_result["command"],
                returncode=linkage_result["returncode"],
                compile_log=compile_result["log"],
                linkage_log=linkage_result["log"]), progress)
            continue

        run_command = (
            [mpirun] + launcher_args +
            ["-n", str(rank_count), str(executable)]
        )
        run_result = _command_result(
            "run_" + name,
            run_command,
            dirs["base"],
            env,
            dirs["logs"] / ("run_" + name + ".json"))
        if run_result["timed_out"]:
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe runtime timed out",
                phase="run",
                source=str(source),
                executable=str(executable),
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                timeout=run_result["timeout"],
                timed_out=run_result["timed_out"],
                compile_log=compile_result["log"],
                run_log=run_result["log"]), progress)
            continue
        skip_reason = case.get("skip_exit_codes", {}).get(
            run_result["returncode"])
        if skip_reason is not None:
            _append_check(checks, _skip(
                check_name,
                skip_reason,
                phase="run",
                source=str(source),
                executable=str(executable),
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"]), progress)
            continue
        if run_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe runtime failed",
                phase="run",
                source=str(source),
                executable=str(executable),
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"]), progress)
            continue

        _append_check(checks, _pass(
            check_name,
            source=str(source),
            executable=str(executable),
            rank_count=rank_count,
            family=case.get("family"),
            api_names=list(case.get("api_names", ())),
            support_api_names=list(case.get("support_api_names", ())),
            compile_command=compile_result["command"],
            run_command=run_result["command"],
            compile_log=compile_result["log"],
            linkage_command=linkage_result["command"],
            linkage_log=linkage_result["log"],
            run_log=run_result["log"]), progress)

    return checks


def _installed_c_probe_checks(srcdir, manifest, tools, dirs, progress=None):
    """Compile, link-inspect, and run installed C ABI runtime probes.

    Before compiling generated probes, this function verifies that probe
    families are nonempty and that the installed ABI header declares the
    metadata constants the probes will reference.  That makes metadata
    drift a clear preflight failure instead of a smaller generated C
    source that silently preserves PASS.
    """
    checks = []
    env = _installed_test_env(tools)
    header = _installed_standard_abi_header(tools, dirs, env)
    include_fortran = _fortran_bindings_enabled(manifest)
    if header is None:
        if progress is not None:
            progress.start("installed_c_probe_metadata_constants")
        _append_check(checks, _skip(
            "installed_c_probe_metadata_constants",
            SKIP_HEADER_UNAVAILABLE), progress)
        return checks

    declared_names = (
        _parse_header_constant_names(header)
    )
    if progress is not None:
        progress.start("installed_c_probe_generation")
    generation_check = _runtime_probe_generation_check(
        manifest, include_fortran, declared_names)
    _append_check(checks, generation_check, progress)
    if generation_check["result"] != "PASS":
        return checks

    expected_names = _runtime_probe_constant_names(
        manifest, include_fortran, declared_names)
    missing_names = sorted(expected_names - declared_names)
    if progress is not None:
        progress.start("installed_c_probe_metadata_constants")
    if missing_names:
        _append_check(checks, _fail(
            "installed_c_probe_metadata_constants",
            "installed standard ABI header is missing probe constants",
            header=str(header),
            missing=missing_names[:20],
            missing_count=len(missing_names),
            expected_count=len(expected_names),
            declared_count=len(declared_names)), progress)
        return checks
    else:
        _append_check(checks, _pass(
            "installed_c_probe_metadata_constants",
            header=str(header),
            checked=len(expected_names),
            declared_count=len(declared_names)), progress)

    checks.extend(_run_installed_c_probe_cases(
        srcdir, manifest, tools, dirs, declared_names,
        INSTALLED_C_ABI_PROBES, progress))

    if progress is not None:
        progress.start("installed_c_runtime_api_probe_generation")
    runtime_api_generation_check = _runtime_api_probe_generation_check(
        srcdir, manifest, header, INSTALLED_C_RUNTIME_API_PROBES)
    _append_check(checks, runtime_api_generation_check, progress)
    if runtime_api_generation_check["result"] != "PASS":
        return checks

    if progress is not None:
        progress.start("installed_c_runtime_api_coverage_audit")
    _append_check(checks, _runtime_api_coverage_audit(
        manifest, header, INSTALLED_C_RUNTIME_API_PROBES), progress)

    checks.extend(_run_installed_c_probe_cases(
        srcdir, manifest, tools, dirs, declared_names,
        INSTALLED_C_RUNTIME_API_PROBES, progress))

    if progress is not None:
        progress.start("installed_c_callback_api_probe_generation")
    callback_api_generation_check = _runtime_api_probe_generation_check(
        srcdir, manifest, header, INSTALLED_C_CALLBACK_PROBES,
        "installed_c_callback_api_probe_generation")
    _append_check(checks, callback_api_generation_check, progress)
    if callback_api_generation_check["result"] != "PASS":
        return checks

    if progress is not None:
        progress.start("installed_c_callback_api_coverage_audit")
    _append_check(checks, _callback_api_coverage_audit(
        manifest, header, INSTALLED_C_CALLBACK_PROBES), progress)

    checks.extend(_run_installed_c_probe_cases(
        srcdir, manifest, tools, dirs, declared_names,
        INSTALLED_C_CALLBACK_PROBES, progress))

    return checks


def run_installed_checks(manifest, mode, srcdir, outdir, tools,
                         progress=None):
    """Run checks that require an installed Open MPI standard ABI build.

    Installed checks intentionally live outside make check's normal
    check_PROGRAMS flow: they need an installed mpicc_abi/mpirun pair and
    create one short-lived executable per probe so MPI runtime failures
    do not contaminate later probes in the same process.
    """
    if mode != "check-abi":
        return []
    dirs = _installed_test_dirs(outdir)
    checks = []
    checks.extend(_installed_wrapper_checks(tools, dirs, progress))
    checks.extend(_installed_c_header_symbol_checks(
        manifest, tools, dirs, progress))
    checks.extend(_installed_c_probe_checks(
        srcdir, manifest, tools, dirs, progress))
    checks.extend(_installed_fortran_checks(
        srcdir, manifest, tools, dirs, progress))
    return checks
