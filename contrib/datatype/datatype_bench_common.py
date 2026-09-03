#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI

"""Helpers shared by the datatype benchmark drivers in this directory.

These functions record the machine and binary identity that every driver
writes into its manifest.  Keeping a single copy means the manifests emitted
by the different tools stay comparable rather than drifting apart.
"""

from __future__ import annotations

import hashlib
import platform
import subprocess
import sys
from pathlib import Path

# Translation units and quoted headers that produce to_self.  Keep this in
# lockstep with to_self_SOURCES in ompi/test/datatype/Makefile.am so an
# out-of-tree mpicc build compiles the same set as the in-tree tester.
TO_SELF_UNITS = ("to_self.c", "datatype_corpus.c")
TO_SELF_HEADERS = ("datatype_corpus.h",)


def to_self_compile_inputs(source: Path) -> tuple[list[Path], list[str]]:
    """Resolve every to_self .c file and the -I for sibling headers.

    ``source`` is the documented to_self.c path; companions are taken from the
    same directory so a future split only needs an update of TO_SELF_UNITS.
    """
    directory = source.expanduser().resolve().parent
    sources: list[Path] = []
    for name in TO_SELF_UNITS:
        path = directory / name
        if not path.is_file():
            raise RuntimeError(
                f"benchmark source {name} is missing beside {source}; "
                f"to_self is built from {' and '.join(TO_SELF_UNITS)}"
            )
        sources.append(path)
    for name in TO_SELF_HEADERS:
        path = directory / name
        if not path.is_file():
            raise RuntimeError(f"benchmark header {name} is missing beside {source}")
    return sources, [f"-I{directory}"]


def sha256(path: Path) -> str:
    """Return a stable content hash that ties a manifest to an exact binary."""
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def cpu_description() -> str:
    """Return a useful processor name on Linux and macOS without external packages."""
    if sys.platform == "darwin":
        # machdep.cpu.brand_string exists on Intel Macs; hw.model and the
        # system_profiler "Chip:" line cover Apple Silicon, where the brand
        # string is absent.
        for key in ("machdep.cpu.brand_string", "hw.model"):
            try:
                result = subprocess.run(["sysctl", "-n", key], text=True, capture_output=True)
            except OSError:
                break
            if 0 == result.returncode and result.stdout.strip():
                return result.stdout.strip()
        try:
            result = subprocess.run(
                ["system_profiler", "SPHardwareDataType"], text=True, capture_output=True
            )
        except OSError:
            pass
        else:
            if 0 == result.returncode:
                for line in result.stdout.splitlines():
                    if line.strip().startswith("Chip:"):
                        return line.split(":", 1)[1].strip()
    cpuinfo = Path("/proc/cpuinfo")
    if cpuinfo.exists():
        for line in cpuinfo.read_text(errors="replace").splitlines():
            if line.lower().startswith("model name"):
                return line.split(":", 1)[1].strip()
    return platform.processor() or "unknown"
