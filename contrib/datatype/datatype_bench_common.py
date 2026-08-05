#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

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
            result = subprocess.run(["sysctl", "-n", key], text=True, capture_output=True)
            if 0 == result.returncode and result.stdout.strip():
                return result.stdout.strip()
        result = subprocess.run(
            ["system_profiler", "SPHardwareDataType"], text=True, capture_output=True
        )
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
