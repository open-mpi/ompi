#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Result aggregation, human-readable summary, and JSON/text
output."""

import json
import os
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    FAIL_CROSS_PROBES_NOT_EXECUTED,
    FAIL_OPEN_MPI_ABI_CLASSIFICATION_UNCONFIRMED,
    SKIP_CROSS_UNSUPPORTED_PLATFORM, SKIP_MPICH_DIRECTIONS_INVALID,
    SKIP_MPICH_TOOLS_UNAVAILABLE, SKIP_OPEN_MPI_TOOLS_UNAVAILABLE,
    SKIP_STANDARD_ABI_DISABLED, _Colors, _append_check, _fail, _write_json,
    _write_text)
from _abi_discovery import (
    _mpich_tools_available, _open_mpi_tool_override_requested,
    _open_mpi_tools_available, _open_mpi_tools_present)
from _abi_installed import (
    _check_counts, _count_by, _language_counts, run_installed_checks)
from _abi_cross import (
    _symbol_diagnostics, _tool_info, run_cross_checks)
from _abi_fast import (
    run_fast_checks)


def _mode_skip_guidance(mode, skip_reason):
    """Return user-facing guidance for mode-level SKIP results."""
    if mode == "check-abi-mpich":
        if skip_reason == SKIP_MPICH_TOOLS_UNAVAILABLE:
            return (
                "MPICH MPI Forum ABI tools were not found.  Build MPICH "
                "with MPI Forum ABI support, ensure mpicc_abi, mpi_abi.h, "
                "and libmpi_abi are installed, put MPICH on PATH, or set "
                "MPICH_ABI_TEST_MPICC and "
                "MPICH_ABI_TEST_MPIRUN.  See test/mpi-abi/README.md for "
                "check-abi-mpich setup."
            )
        if skip_reason == SKIP_OPEN_MPI_TOOLS_UNAVAILABLE:
            return (
                "Open MPI ABI tools were not found.  Install Open MPI with "
                "standard ABI support, put its tools on PATH, or set "
                "OMPI_ABI_TEST_MPICC_ABI and OMPI_ABI_TEST_MPIRUN.  See "
                "test/mpi-abi/README.md for check-abi-mpich setup."
            )
        if skip_reason == SKIP_CROSS_UNSUPPORTED_PLATFORM:
            return (
                "This platform does not expose a supported runtime library "
                "path environment variable for MPICH compatibility tests.  "
                "See test/mpi-abi/README.md for check-abi-mpich setup."
            )
        if skip_reason == SKIP_MPICH_DIRECTIONS_INVALID:
            return (
                "OMPI_ABI_TEST_MPICH_DIRECTIONS contains an invalid value.  "
                "Use both, mpich-to-ompi, ompi-to-mpich, "
                "mpich_compile_open_mpi_run, or "
                "open_mpi_compile_mpich_run."
            )
    return None


def _mode_failure_guidance(mode, failure_reason):
    """Return user-facing guidance for mode-level FAIL results."""
    if mode == "check-abi-mpich":
        if failure_reason == SKIP_STANDARD_ABI_DISABLED:
            return (
                "Open MPI was configured without standard ABI support.  "
                "Reconfigure and install Open MPI with standard ABI support "
                "before running make check-abi-mpich."
            )
        if failure_reason == SKIP_MPICH_TOOLS_UNAVAILABLE:
            return (
                "MPICH MPI Forum ABI tools were not found.  Build MPICH "
                "with MPI Forum ABI support, ensure mpicc_abi, mpi_abi.h, "
                "and libmpi_abi are installed, put MPICH on PATH, or set "
                "MPICH_ABI_TEST_MPICC and "
                "MPICH_ABI_TEST_MPIRUN.  See test/mpi-abi/README.md for "
                "check-abi-mpich setup."
            )
        if failure_reason == SKIP_OPEN_MPI_TOOLS_UNAVAILABLE:
            return (
                "Open MPI ABI tools were not found.  Install Open MPI with "
                "standard ABI support, put its tools on PATH, or set "
                "OMPI_ABI_TEST_MPICC_ABI and OMPI_ABI_TEST_MPIRUN.  See "
                "test/mpi-abi/README.md for check-abi-mpich setup."
            )
        if failure_reason == FAIL_OPEN_MPI_ABI_CLASSIFICATION_UNCONFIRMED:
            return (
                "Open MPI ABI tools were found but could not be validated "
                "as an Open MPI MPI Forum ABI installation.  Check the "
                "reported discovery evidence, wrapper link flags, standard "
                "ABI header path, and OMPI_ABI_TEST_* overrides."
            )
        if failure_reason == SKIP_CROSS_UNSUPPORTED_PLATFORM:
            return (
                "This platform does not expose a supported runtime library "
                "path environment variable for MPICH compatibility tests.  "
                "See test/mpi-abi/README.md for check-abi-mpich setup."
            )
        if failure_reason == SKIP_MPICH_DIRECTIONS_INVALID:
            return (
                "OMPI_ABI_TEST_MPICH_DIRECTIONS contains an invalid value.  "
                "Use both, mpich-to-ompi, ompi-to-mpich, "
                "mpich_compile_open_mpi_run, or "
                "open_mpi_compile_mpich_run."
            )
        if failure_reason == FAIL_CROSS_PROBES_NOT_EXECUTED:
            return (
                "No MPICH compatibility cross probes executed successfully.  "
                "Inspect the per-probe SKIP reasons and command logs under "
                "the ABI test output directory."
            )
    return None


def _append_mode_failure(checks, progress, name, reason, message, **details):
    """Append a FAIL check for an explicit mode prerequisite failure."""
    guidance = _mode_failure_guidance("check-abi-mpich", reason)
    if guidance is not None:
        details["guidance"] = guidance
    _append_check(checks, _fail(name, message, reason=reason, **details),
                  progress)
    return guidance


def build_report(manifest, mode, srcdir, builddir, outdir, progress=None):
    """Run the requested mode and assemble the machine-readable report.

    Mode-level SKIPs are decided before running subordinate checks so a
    build without standard ABI support remains a spec-sanctioned skip
    rather than failing completion gates or installed-tool discovery.
    """
    api_entries = manifest["apis"]
    constant_entries = manifest["constants"]
    standard_abi = manifest["configuration"]["standard_abi"]
    classifications = _count_by(api_entries, "classification")
    test_status = _count_by(api_entries, "test_status")
    constant_classifications = _count_by(constant_entries, "classification")
    constant_test_status = _count_by(constant_entries, "test_status")

    skip_reason = None
    failure_guidance = None
    result = "PASS"
    tools = _tool_info(mode)
    fast_checks = []
    installed_checks = []
    cross_checks = []
    cross_environment = None

    if mode in ("coverage", "check-fast", "check-abi"):
        if standard_abi["enabled"] is False:
            result = "SKIP"
            skip_reason = SKIP_STANDARD_ABI_DISABLED

    if result != "SKIP" and mode in ("coverage", "check-fast"):
        fast_checks = run_fast_checks(manifest, srcdir, builddir, progress)
        if any(check["result"] == "FAIL" for check in fast_checks):
            result = "FAIL"

    if result != "SKIP" and mode == "check-abi":
        if not _open_mpi_tools_available(tools):
            if (_open_mpi_tool_override_requested() or
                    _open_mpi_tools_present(tools)):
                result = "FAIL"
                reason = FAIL_OPEN_MPI_ABI_CLASSIFICATION_UNCONFIRMED
                message = (
                    "Open MPI ABI tools were found but could not be "
                    "validated for ABI testing"
                )
                if _open_mpi_tool_override_requested():
                    message = "explicit Open MPI ABI tool override is invalid"
                _append_check(installed_checks, _fail(
                    "installed_open_mpi_prerequisites",
                    message,
                    reason=reason,
                    open_mpi_tools=tools["open_mpi"],
                    discovery=tools.get("discovery", {})), progress)
            else:
                result = "SKIP"
                skip_reason = SKIP_OPEN_MPI_TOOLS_UNAVAILABLE
        else:
            installed_checks = run_installed_checks(
                manifest, mode, srcdir, outdir, tools, progress)
            if any(check["result"] == "FAIL"
                   for check in installed_checks):
                result = "FAIL"

    if mode == "check-abi-mpich":
        if standard_abi["enabled"] is False:
            result = "FAIL"
            failure_guidance = _append_mode_failure(
                cross_checks,
                progress,
                "mpich_prerequisites",
                SKIP_STANDARD_ABI_DISABLED,
                "standard ABI support is required for check-abi-mpich",
                standard_abi=standard_abi)
        elif tools["cross"]["direction_error"]:
            result = "FAIL"
            failure_guidance = _append_mode_failure(
                cross_checks,
                progress,
                "mpich_prerequisites",
                SKIP_MPICH_DIRECTIONS_INVALID,
                "invalid OMPI_ABI_TEST_MPICH_DIRECTIONS value",
                raw_value=tools["cross"]["direction_raw_value"],
                invalid_values=tools["cross"]["direction_invalid_values"])
        elif tools["cross"]["runtime_loader"] is None:
            result = "FAIL"
            failure_guidance = _append_mode_failure(
                cross_checks,
                progress,
                "mpich_prerequisites",
                SKIP_CROSS_UNSUPPORTED_PLATFORM,
                "unsupported platform for MPICH compatibility tests",
                platform=tools["cross"]["platform"])
        elif not _open_mpi_tools_available(tools):
            result = "FAIL"
            reason = SKIP_OPEN_MPI_TOOLS_UNAVAILABLE
            message = "Open MPI ABI tools are required for check-abi-mpich"
            if _open_mpi_tools_present(tools):
                reason = FAIL_OPEN_MPI_ABI_CLASSIFICATION_UNCONFIRMED
                message = (
                    "Open MPI ABI tools were found but could not be "
                    "validated for check-abi-mpich"
                )
            failure_guidance = _append_mode_failure(
                cross_checks,
                progress,
                "mpich_prerequisites",
                reason,
                message,
                open_mpi_tools=tools["open_mpi"],
                discovery=tools.get("discovery", {}))
        elif not _mpich_tools_available(tools):
            result = "FAIL"
            failure_guidance = _append_mode_failure(
                cross_checks,
                progress,
                "mpich_prerequisites",
                SKIP_MPICH_TOOLS_UNAVAILABLE,
                "MPICH MPI Forum ABI tools are required for "
                "check-abi-mpich",
                mpich_tools=tools["mpich"])
        else:
            cross_checks, cross_environment = run_cross_checks(
                manifest, srcdir, tools, outdir, progress)
            if any(check["result"] == "FAIL" for check in cross_checks):
                result = "FAIL"
            else:
                cross_summary = next(
                    (check for check in cross_checks
                     if check["name"] == "cross_direction_summary"),
                    None)
                if (cross_summary is not None and
                        cross_summary["result"] == "SKIP"):
                    result = "FAIL"
                    failure_guidance = _append_mode_failure(
                        cross_checks,
                        progress,
                        "mpich_cross_probes_executed",
                        FAIL_CROSS_PROBES_NOT_EXECUTED,
                        "no MPICH compatibility cross probes executed "
                        "successfully",
                        cross_direction_summary=cross_summary)

    return {
        "mode": mode,
        "result": result,
        "skip_reason": skip_reason,
        "skip_guidance": _mode_skip_guidance(mode, skip_reason),
        "failure_guidance": failure_guidance,
        "srcdir": str(srcdir),
        "builddir": str(builddir),
        "tmpdir": os.environ.get("OMPI_ABI_TEST_TMPDIR")
                  or tempfile.gettempdir(),
        "tools": tools,
        "symbol_diagnostics": _symbol_diagnostics(),
        "fast_checks": fast_checks,
        "installed_checks": installed_checks,
        "cross_checks": cross_checks,
        "cross_environment": cross_environment,
        "summary": {
            "apis_total": len(api_entries),
            "api_classifications": classifications,
            "api_test_status": test_status,
            "constants_total": len(constant_entries),
            "constant_classifications": constant_classifications,
            "constant_test_status": constant_test_status,
            "fast_check_status": _check_counts(fast_checks),
            "installed_check_status": _check_counts(installed_checks),
            "cross_check_status": _check_counts(cross_checks),
            "language_coverage": _language_counts(api_entries),
        },
    }


def _summary_text(report, colors=None):
    """Render a human-readable summary from a report object."""
    if colors is None:
        colors = _Colors(False)
    lines = []
    lines.append("MPI ABI test summary")
    lines.append("====================")
    lines.append("mode: {0}".format(report["mode"]))
    result_text = "result: {0}".format(report["result"])
    lines.append(colors.result(report["result"], result_text))
    if report["skip_reason"]:
        lines.append("skip_reason: {0}".format(report["skip_reason"]))
    if report.get("skip_guidance"):
        lines.append("skip_guidance: {0}".format(report["skip_guidance"]))
    if report.get("failure_guidance"):
        lines.append("failure_guidance: {0}".format(
            report["failure_guidance"]))
    if report.get("completion_gate") is not None:
        gate = report["completion_gate"]
        lines.append("completion_gate: {0}".format(gate["result"]))
        if gate.get("skip_preserved"):
            lines.append("completion_gate_skip_preserved: true")
        if gate["findings"]:
            lines.append("completion_gate_findings:")
            for finding in gate["findings"][:20]:
                lines.append("  {0}: {1}".format(
                    finding["kind"], finding["message"]))
    lines.append("srcdir: {0}".format(report["srcdir"]))
    lines.append("builddir: {0}".format(report["builddir"]))
    lines.append("tmpdir: {0}".format(report["tmpdir"]))
    lines.append("")
    lines.append("API entries: {0}".format(report["summary"]["apis_total"]))
    for key, value in sorted(report["summary"]["api_classifications"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("API test status:")
    for key, value in sorted(report["summary"]["api_test_status"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("ABI constants: {0}".format(
        report["summary"]["constants_total"]))
    for key, value in sorted(
            report["summary"]["constant_classifications"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("ABI constant test status:")
    for key, value in sorted(
            report["summary"]["constant_test_status"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("Fast checks:")
    if report["fast_checks"]:
        for check in report["fast_checks"]:
            line = "  {0}: {1}".format(check["name"], check["result"])
            if check["skip_reason"]:
                line += " ({0})".format(check["skip_reason"])
            lines.append(colors.result(check["result"], line))
    else:
        lines.append("  none")
    lines.append("")
    lines.append("Installed checks:")
    if report["installed_checks"]:
        for check in report["installed_checks"]:
            line = "  {0}: {1}".format(check["name"], check["result"])
            if check["skip_reason"]:
                line += " ({0})".format(check["skip_reason"])
            lines.append(colors.result(check["result"], line))
    else:
        lines.append("  none")
    lines.append("")
    lines.append("Cross checks:")
    if report["cross_checks"]:
        for check in report["cross_checks"]:
            line = "  {0}: {1}".format(check["name"], check["result"])
            if check["skip_reason"]:
                line += " ({0})".format(check["skip_reason"])
            lines.append(colors.result(check["result"], line))
    else:
        lines.append("  none")
    lines.append("")
    if report.get("cross_environment") is not None:
        env = report["cross_environment"]
        lines.append("Cross environment:")
        lines.append("  platform: {0}".format(env["platform"]))
        lines.append("  runtime_loader: {0}".format(env["runtime_loader"]))
        lines.append("  directions: {0}".format(
            ", ".join(env["directions"]) if env["directions"] else "none"))
        for name, tool in sorted(env["tools"].items()):
            version = tool.get("probes", {}).get("version", {})
            version_line = version.get("stdout_first_line") or \
                version.get("stderr_first_line") or "unavailable"
            lines.append("  {0}: {1}".format(name, tool["path"]))
            lines.append("    version: {0}".format(version_line))
        lines.append("")
    lines.append("Language coverage:")
    for key, value in sorted(report["summary"]["language_coverage"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("Tools:")
    lines.append(json.dumps(report["tools"], indent=2, sort_keys=True))
    lines.append("")
    lines.append("Symbol diagnostics:")
    lines.append(json.dumps(
        report["symbol_diagnostics"], indent=2, sort_keys=True))
    lines.append("")
    return "\n".join(lines)


def write_outputs(manifest, report, outdir):
    """Write manifest, JSON report, and plain-text summary files."""
    outdir.mkdir(parents=True, exist_ok=True)
    manifest_path = outdir / "abi-manifest.json"
    report_path = outdir / "abi-report.json"
    summary_path = outdir / "abi-summary.txt"
    _write_json(manifest_path, manifest)
    _write_json(report_path, report)
    _write_text(summary_path, _summary_text(report))
    return manifest_path, report_path, summary_path
