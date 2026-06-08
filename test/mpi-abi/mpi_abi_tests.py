#!/usr/bin/env python3
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""MPI standard ABI test manifest and runner.

This first implementation provides the phase 1-4 infrastructure:
metadata loading, manifest generation, tool discovery, skip handling,
and JSON/text reporting.  Later phases add generated compile, link, and
runtime tests on top of this runner.
"""

import argparse
import json
import os
from pathlib import Path
import platform
import re
import shutil
import sys
import tempfile


CLASS_IMPLEMENTED = "implemented"
CLASS_NOT_IMPLEMENTED = "not_implemented"
CLASS_NOT_IN_STANDARD_ABI = "not_in_standard_abi"
CLASS_UNSUPPORTED_BY_BUILD = "unsupported_by_build"
CLASS_UNSUPPORTED_BY_OPEN_MPI = "unsupported_by_open_mpi"

TEST_NOT_WRITTEN = "test_not_written_yet"
TEST_NOT_APPLICABLE = "not_applicable"
TEST_CALLBACK_DEFERRED = "callback_deferred"

SKIP_STANDARD_ABI_DISABLED = "standard_abi_disabled"
SKIP_OPEN_MPI_TOOLS_UNAVAILABLE = "open_mpi_tools_unavailable"
SKIP_MPICH_TOOLS_UNAVAILABLE = "mpich_tools_unavailable"

EXPECTED_METADATA_VERSION = "5.0"
EXPECTED_API_COUNT = 567
EXPECTED_CONSTANT_COUNT = 373


def _read_json(path):
    with path.open("r", encoding="utf-8") as stream:
        return json.load(stream)


def _write_json(path, payload):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as stream:
        json.dump(payload, stream, indent=2, sort_keys=True)
        stream.write("\n")


def _write_text(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as stream:
        stream.write(text)


def _env_bool(name):
    value = os.environ.get(name)
    if value is None:
        return None
    return value.strip().lower() in ("1", "true", "yes", "on")


def _which(env_name, default_name):
    override = os.environ.get(env_name)
    if override:
        return override
    return shutil.which(default_name)


def _api_stem(api_key, api_name):
    name = api_key
    if name.startswith("mpi_"):
        name = name[4:]
    elif api_name.startswith("MPI_"):
        name = api_name[4:].lower()
    return name


def _api_family(name):
    lower = name.lower()
    if lower.startswith("mpi_t_"):
        return "mpi_t"
    if lower.startswith("mpi_abi_"):
        return "abi"
    parts = lower.split("_")
    if len(parts) < 2:
        return "misc"
    if parts[1] in ("comm", "group", "type", "file", "win", "session"):
        return parts[1]
    if parts[1] in ("send", "recv", "isend", "irecv", "probe", "iprobe"):
        return "point_to_point"
    if parts[1] in ("allreduce", "allgather", "allgatherv", "alltoall"):
        return "collective"
    if parts[1] in ("barrier", "bcast", "gather", "gatherv"):
        return "collective"
    if parts[1] in ("reduce", "scan", "exscan"):
        return "collective"
    if parts[1] in ("put", "get", "accumulate", "rput", "rget"):
        return "rma"
    if parts[1] in ("init", "finalize", "initialized", "finalized"):
        return "init"
    if parts[1] in ("errhandler", "error"):
        return "error"
    return parts[1]


def _rank_requirement(family):
    if family in ("collective", "point_to_point"):
        return 2
    return 1


def _feature_requirement(family):
    if family == "file":
        return "mpi_io"
    if family == "mpi_t":
        return "mpi_t"
    if family == "win" or family == "rma":
        return "rma"
    if family == "session":
        return "sessions"
    return None


def _find_standard_abi_setting(srcdir, builddir):
    override = _env_bool("OMPI_ABI_TEST_STANDARD_ABI")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_STANDARD_ABI",
        }

    conditional = _conditional_enabled(builddir, "OMPI_STANDARD_ABI")
    if conditional["enabled"] is not None:
        return conditional

    return {
        "enabled": None,
        "source": "unknown",
    }


def _conditional_enabled(builddir, conditional):
    config_status = builddir / "config.status"
    if not config_status.exists():
        return {
            "enabled": None,
            "source": "unknown",
        }

    text = config_status.read_text(encoding="utf-8", errors="ignore")
    true_name = conditional + "_TRUE"
    false_name = conditional + "_FALSE"
    true_pattern = r'S\["' + re.escape(true_name) + r'"\]="([^"]*)"'
    false_pattern = r'S\["' + re.escape(false_name) + r'"\]="([^"]*)"'
    true_match = re.search(true_pattern, text)
    false_match = re.search(false_pattern, text)
    if true_match is not None:
        return {
            "enabled": true_match.group(1) == "",
            "source": str(config_status),
        }
    if false_match is not None:
        return {
            "enabled": false_match.group(1) != "",
            "source": str(config_status),
        }
    return {
        "enabled": None,
        "source": str(config_status),
    }


def _detect_fortran_support(builddir):
    conditionals = {
        "mpif.h": "OMPI_BUILD_FORTRAN_MPIFH_BINDINGS",
        "use mpi": "OMPI_BUILD_FORTRAN_USEMPI_BINDINGS",
        "use mpi_f08": "OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS",
    }
    detected = {}
    for language, conditional in conditionals.items():
        detected[language] = _conditional_enabled(builddir, conditional)
    return detected


def _detect_optional_features(builddir):
    # No configured optional MPI API-family feature currently maps
    # cleanly to a standard ABI unsupported_by_build classification.
    return {}


def _abi_metadata_version(abi):
    mpi = abi.get("metadata", {}).get("mpi", {})
    version = mpi.get("version")
    subversion = mpi.get("subversion")
    if version is None or subversion is None:
        return None
    return "{0}.{1}".format(version, subversion)


def _source_exists(srcdir, stem):
    c_dir = srcdir / "ompi" / "mpi" / "c"
    tool_dir = srcdir / "ompi" / "mpi" / "tool"
    candidates = (
        c_dir / (stem + "_abi_generated.c"),
        c_dir / (stem + "_abi.c"),
        c_dir / (stem + ".c.in"),
        tool_dir / (stem + "_abi_generated.c"),
        tool_dir / (stem + ".c.in"),
        tool_dir / (stem + ".c"),
    )
    return any(candidate.exists() for candidate in candidates)


def _classify_api(srcdir, api_key, api):
    attrs = api.get("attributes", {})
    name = api.get("name", api_key)
    stem = _api_stem(api_key, name)
    family = _api_family(name)
    c_expressible = bool(attrs.get("c_expressible"))
    f90_expressible = bool(attrs.get("f90_expressible"))
    f08_expressible = bool(attrs.get("f08_expressible"))
    mpif_expressible = bool(attrs.get("lis_expressible"))
    callback = bool(attrs.get("callback")) or any(
        param.get("func_type") for param in api.get("parameters", [])
    )

    if not any((c_expressible, f90_expressible, f08_expressible,
                mpif_expressible)):
        classification = CLASS_NOT_IN_STANDARD_ABI
        test_status = TEST_NOT_APPLICABLE
    elif _source_exists(srcdir, stem):
        classification = CLASS_IMPLEMENTED
        test_status = TEST_NOT_WRITTEN
    elif callback:
        classification = CLASS_NOT_IMPLEMENTED
        test_status = TEST_CALLBACK_DEFERRED
    else:
        classification = CLASS_NOT_IMPLEMENTED
        test_status = TEST_NOT_APPLICABLE

    skip_reason = None
    if classification != CLASS_IMPLEMENTED:
        skip_reason = classification

    return {
        "kind": "api",
        "key": api_key,
        "name": name,
        "stem": stem,
        "family": family,
        "classification": classification,
        "test_status": test_status,
        "skip_reason": skip_reason,
        "rank_count": _rank_requirement(family),
        "feature": _feature_requirement(family),
        "callback": callback,
        "requires_callback_test": callback,
        "languages": {
            "c": c_expressible,
            "mpif.h": (
                mpif_expressible and not bool(attrs.get("not_with_mpif"))
            ),
            "use mpi": f90_expressible,
            "use mpi_f08": f08_expressible,
        },
    }


def _classify_constant(key, constant):
    return {
        "kind": "constant",
        "key": key,
        "name": constant.get("name", key),
        "category": constant.get("category"),
        "classification": CLASS_IMPLEMENTED,
        "test_status": TEST_NOT_WRITTEN,
        "skip_reason": None,
    }


def load_metadata(srcdir):
    api_path = srcdir / "docs" / "mpi-standard-apis.json"
    abi_path = srcdir / "docs" / "mpi-standard-abi.json"
    api_resolved = api_path.resolve()
    abi_resolved = abi_path.resolve()
    apis = _read_json(api_path)
    abi = _read_json(abi_path)
    if not isinstance(apis, dict):
        raise RuntimeError("MPI API metadata must be a JSON object")
    if not isinstance(abi, dict):
        raise RuntimeError("MPI ABI metadata must be a JSON object")
    constants = abi.get("constants", {})
    if not isinstance(constants, dict):
        raise RuntimeError("MPI ABI constants metadata must be a JSON object")
    abi_version = _abi_metadata_version(abi)
    if abi_version != EXPECTED_METADATA_VERSION:
        raise RuntimeError(
            "unexpected MPI ABI metadata version: {0}".format(abi_version))
    if len(apis) != EXPECTED_API_COUNT:
        raise RuntimeError(
            "unexpected MPI API metadata entry count: {0} != {1}".format(
                len(apis), EXPECTED_API_COUNT))
    if len(constants) != EXPECTED_CONSTANT_COUNT:
        raise RuntimeError(
            "unexpected MPI ABI constant metadata entry count: {0} != {1}".
            format(len(constants), EXPECTED_CONSTANT_COUNT))
    return {
        "api_path": str(api_path),
        "abi_path": str(abi_path),
        "api_resolved_path": str(api_resolved),
        "abi_resolved_path": str(abi_resolved),
        "api_version": None,
        "api_version_source": "not present in API metadata",
        "abi_version": abi_version,
        "abi_version_source": "metadata.mpi",
        "expected_api_count": EXPECTED_API_COUNT,
        "expected_constant_count": EXPECTED_CONSTANT_COUNT,
        "apis": apis,
        "constants": constants,
        "abi_metadata": abi.get("metadata", {}),
    }


def build_manifest(srcdir, builddir):
    metadata = load_metadata(srcdir)
    standard_abi = _find_standard_abi_setting(srcdir, builddir)
    fortran = _detect_fortran_support(builddir)
    optional_features = _detect_optional_features(builddir)

    api_entries = [
        _classify_api(srcdir, key, api)
        for key, api in sorted(metadata["apis"].items())
    ]
    constant_entries = [
        _classify_constant(key, constant)
        for key, constant in sorted(metadata["constants"].items())
    ]

    return {
        "metadata": {
            "api_path": metadata["api_path"],
            "abi_path": metadata["abi_path"],
            "api_resolved_path": metadata["api_resolved_path"],
            "abi_resolved_path": metadata["abi_resolved_path"],
            "api_version": metadata["api_version"],
            "api_version_source": metadata["api_version_source"],
            "abi_version": metadata["abi_version"],
            "abi_version_source": metadata["abi_version_source"],
            "versions_match": (
                metadata["api_version"] == metadata["abi_version"]
                if metadata["api_version"] is not None else None
            ),
            "expected_api_count": metadata["expected_api_count"],
            "expected_constant_count": metadata["expected_constant_count"],
            "abi_metadata": metadata["abi_metadata"],
        },
        "configuration": {
            "standard_abi": standard_abi,
            "fortran": fortran,
            "optional_features": optional_features,
        },
        "apis": api_entries,
        "constants": constant_entries,
    }


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


def _tool_info(mode):
    tools = {
        "open_mpi": {
            "mpicc_abi": _which("OMPI_ABI_TEST_MPICC_ABI", "mpicc_abi"),
            "mpirun": _which("OMPI_ABI_TEST_MPIRUN", "mpirun"),
        },
        "mpich": {
            "mpicc": _which("MPICH_ABI_TEST_MPICC", "mpicc"),
            "mpirun": _which("MPICH_ABI_TEST_MPIRUN", "mpirun"),
        },
        "rank_counts": {
            "np1": int(os.environ.get("OMPI_ABI_TEST_NP1", "1")),
            "np2": int(os.environ.get("OMPI_ABI_TEST_NP2", "2")),
        },
        "paths": {
            "include": os.environ.get("OMPI_ABI_TEST_INCLUDE_PATH"),
            "library": os.environ.get("OMPI_ABI_TEST_LIBRARY_PATH"),
            "launcher_args": os.environ.get("OMPI_ABI_TEST_LAUNCHER_ARGS"),
            "runtime_loader": _runtime_loader_var(),
        },
    }
    if mode != "check-abi-cross":
        tools.pop("mpich")
    return tools


def _symbol_diagnostics():
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
    system = platform.system()
    if system == "Darwin":
        return "DYLD_LIBRARY_PATH"
    if system == "Linux":
        return "LD_LIBRARY_PATH"
    return None


def build_report(manifest, mode, srcdir, builddir):
    api_entries = manifest["apis"]
    constant_entries = manifest["constants"]
    standard_abi = manifest["configuration"]["standard_abi"]
    classifications = _count_by(api_entries, "classification")
    test_status = _count_by(api_entries, "test_status")
    constant_classifications = _count_by(constant_entries, "classification")
    constant_test_status = _count_by(constant_entries, "test_status")

    skip_reason = None
    result = "PASS"
    tools = _tool_info(mode)

    if mode in ("coverage", "check-fast", "check-abi", "check-abi-cross"):
        if standard_abi["enabled"] is False:
            result = "SKIP"
            skip_reason = SKIP_STANDARD_ABI_DISABLED

    if result != "SKIP" and mode == "check-abi":
        ompi_tools = tools["open_mpi"]
        if not ompi_tools["mpicc_abi"] or not ompi_tools["mpirun"]:
            result = "SKIP"
            skip_reason = SKIP_OPEN_MPI_TOOLS_UNAVAILABLE

    if result != "SKIP" and mode == "check-abi-cross":
        ompi_tools = tools["open_mpi"]
        mpich_tools = tools["mpich"]
        if not ompi_tools["mpicc_abi"] or not ompi_tools["mpirun"]:
            result = "SKIP"
            skip_reason = SKIP_OPEN_MPI_TOOLS_UNAVAILABLE
        elif not mpich_tools["mpicc"] or not mpich_tools["mpirun"]:
            result = "SKIP"
            skip_reason = SKIP_MPICH_TOOLS_UNAVAILABLE

    return {
        "mode": mode,
        "result": result,
        "skip_reason": skip_reason,
        "srcdir": str(srcdir),
        "builddir": str(builddir),
        "tmpdir": os.environ.get("OMPI_ABI_TEST_TMPDIR")
                  or tempfile.gettempdir(),
        "tools": tools,
        "symbol_diagnostics": _symbol_diagnostics(),
        "summary": {
            "apis_total": len(api_entries),
            "api_classifications": classifications,
            "api_test_status": test_status,
            "constants_total": len(constant_entries),
            "constant_classifications": constant_classifications,
            "constant_test_status": constant_test_status,
            "language_coverage": _language_counts(api_entries),
        },
    }


def _summary_text(report):
    lines = []
    lines.append("MPI ABI test summary")
    lines.append("====================")
    lines.append("mode: {0}".format(report["mode"]))
    lines.append("result: {0}".format(report["result"]))
    if report["skip_reason"]:
        lines.append("skip_reason: {0}".format(report["skip_reason"]))
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
    outdir.mkdir(parents=True, exist_ok=True)
    manifest_path = outdir / "abi-manifest.json"
    report_path = outdir / "abi-report.json"
    summary_path = outdir / "abi-summary.txt"
    _write_json(manifest_path, manifest)
    _write_json(report_path, report)
    _write_text(summary_path, _summary_text(report))
    return manifest_path, report_path, summary_path


def command(args):
    srcdir = Path(args.srcdir).resolve()
    builddir = Path(args.builddir).resolve()
    outdir = (
        Path(args.outdir).resolve() if args.outdir else builddir / ".mpi-abi"
    )
    manifest = build_manifest(srcdir, builddir)
    mode = args.mode

    if mode == "manifest":
        outdir.mkdir(parents=True, exist_ok=True)
        path = outdir / "abi-manifest.json"
        _write_json(path, manifest)
        print("Wrote {0}".format(path))
        return 0

    report = build_report(manifest, mode, srcdir, builddir)
    _, report_path, summary_path = write_outputs(manifest, report, outdir)
    print(_summary_text(report), end="")
    print("Wrote {0}".format(report_path))
    print("Wrote {0}".format(summary_path))

    if args.complete_gate and report["result"] != "SKIP":
        api_not_written = report["summary"]["api_test_status"].get(
            TEST_NOT_WRITTEN, 0)
        constant_not_written = report["summary"][
            "constant_test_status"].get(TEST_NOT_WRITTEN, 0)
        not_written = api_not_written + constant_not_written
        if not_written:
            print(
                "ERROR: completion gate found {0} unwritten tests "
                "({1} APIs, {2} constants)".format(
                    not_written, api_not_written, constant_not_written),
                file=sys.stderr)
            return 1

    return 0


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--srcdir", default=Path(__file__).parents[2],
                        help="top source directory")
    parser.add_argument("--builddir", default=os.getcwd(),
                        help="top build directory")
    parser.add_argument("--outdir",
                        help="output directory for generated reports")
    parser.add_argument("--complete-gate", action="store_true",
                        help="fail when implemented ABI entries lack tests")
    parser.add_argument("mode", choices=(
        "manifest",
        "coverage",
        "check-fast",
        "check-abi",
        "check-abi-cross",
    ))
    args = parser.parse_args(argv)
    try:
        return command(args)
    except Exception as exc:
        print("ERROR: {0}".format(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
