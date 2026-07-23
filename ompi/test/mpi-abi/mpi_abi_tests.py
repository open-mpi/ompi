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

This implementation provides the phase 1-9 infrastructure: metadata
loading, manifest generation, fast metadata checks, installed ABI smoke
tests, installed C header/prototype/symbol checks, C converter probes,
seed C runtime API-family probes, tool discovery, skip handling, and
JSON/text reporting.  Later phases add exhaustive runtime tests on top
of this runner.
"""

import argparse
import os
from pathlib import Path
import sys
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    _Colors, _Progress, _color_tests_enabled, _write_json)
from _abi_discovery import (
    _env_bool)
from _abi_manifest import (
    build_manifest)
from _abi_fast import (
    _completion_gate_report)
from _abi_report import (
    _summary_text, build_report, write_outputs)


def _default_outdir(mode, builddir):
    """Return the default generated-output directory for one mode."""
    if mode in ("check-fast", "coverage"):
        return builddir / "check-results"
    if mode == "check-abi":
        return builddir / "check-abi-results"
    if mode == "check-abi-mpich":
        return builddir / "check-abi-mpich-results"
    return builddir / "check-results"


def command(args):
    """Execute the parsed CLI command and return a shell exit status.

    Reports are written for every mode, including failures, because the
    JSON command logs are the primary diagnostic artifact for CI and for
    reviewer investigations.  The completion gate is applied after the
    report is built and deliberately ignored for skipped runs.
    """
    srcdir = Path(args.srcdir).resolve()
    builddir = Path(args.builddir).resolve()
    mode = args.mode
    outdir = (
        Path(args.outdir).resolve() if args.outdir
        else _default_outdir(mode, builddir)
    )
    manifest = build_manifest(srcdir, builddir)

    if mode == "manifest":
        outdir.mkdir(parents=True, exist_ok=True)
        path = outdir / "abi-manifest.json"
        _write_json(path, manifest)
        print("Wrote {0}".format(path))
        return 0

    progress_enabled = args.progress
    env_progress = _env_bool("OMPI_ABI_TEST_PROGRESS")
    if env_progress is not None:
        progress_enabled = env_progress
    colors = _Colors(_color_tests_enabled(args.color_tests))
    progress = _Progress(progress_enabled, colors)

    report = build_report(manifest, mode, srcdir, builddir, outdir, progress)
    if args.complete_gate:
        report["completion_gate"] = _completion_gate_report(manifest, report)
    _, report_path, summary_path = write_outputs(manifest, report, outdir)
    if progress.enabled:
        print("")
    print(_summary_text(report, colors), end="")
    print("Wrote {0}".format(report_path))
    print("Wrote {0}".format(summary_path))

    if args.complete_gate and report["completion_gate"]["result"] == "FAIL":
        print(
            "ERROR: completion gate found {0} finding(s)".format(
                report["completion_gate"]["finding_count"]),
            file=sys.stderr)
        return 1

    if report["result"] == "FAIL":
        return 1

    return 0


def _default_srcdir():
    """Return the top source tree for direct runner invocations.

    Automake passes --srcdir explicitly, but argparse still evaluates
    defaults before parsing the command line.  Python 3.7 keeps
    Path(__file__) relative when the script is invoked as
    ./mpi_abi_tests.py, so resolve it before indexing parents.

    This file is ompi/test/mpi-abi/mpi_abi_tests.py, so the top of the
    source tree is three levels up.
    """
    return Path(__file__).resolve().parents[3]


def main(argv):
    """Parse command-line arguments and run the MPI ABI test runner."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--srcdir", default=_default_srcdir(),
                        help="top source directory")
    parser.add_argument("--builddir", default=os.getcwd(),
                        help="top build directory")
    parser.add_argument("--outdir",
                        help="output directory for generated reports")
    parser.add_argument("--complete-gate", action="store_true",
                        help="fail when implemented ABI entries lack tests")
    parser.add_argument("--no-progress", dest="progress",
                        action="store_false",
                        help="do not print per-check progress lines")
    parser.set_defaults(progress=True)
    parser.add_argument("--color-tests", default="auto",
                        choices=("auto", "yes", "no", "always", "never"),
                        help="colorize console test results")
    parser.add_argument("mode", choices=(
        "manifest",
        "coverage",
        "check-fast",
        "check-abi",
        "check-abi-mpich",
    ))
    args = parser.parse_args(argv)
    try:
        return command(args)
    except RuntimeError as exc:
        # RuntimeError is the runner's intentional gate: metadata
        # version/count mismatches, malformed probe tables, and invalid
        # operator environment values.  A clean one-line message is the
        # right diagnostic for those.
        print("ERROR: {0}".format(exc), file=sys.stderr)
        return 1
    except Exception:
        # Any other exception is an unexpected bug in the harness itself
        # (KeyError, TypeError, AttributeError, ...).  Preserve the full
        # traceback so a CI failure of the runner is diagnosable instead of
        # being collapsed into an opaque one-liner.
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
