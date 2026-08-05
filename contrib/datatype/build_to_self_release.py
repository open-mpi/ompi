#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

"""Build one to_self source with an MPI compiler wrapper and optional Open MPI build tree."""

from __future__ import annotations

import argparse
import json
import os
import shlex
import subprocess
import sys
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, required=True, help="benchmark to_self.c")
    parser.add_argument("--release-source", type=Path, help="matching Open MPI source root")
    parser.add_argument("--release-build", type=Path, help="configured Open MPI build root")
    parser.add_argument("--mpicc", type=Path, required=True, help="mpicc installed by that release")
    parser.add_argument("--output", type=Path, required=True, help="output benchmark executable")
    parser.add_argument("--cflags", default="-O3 -DNDEBUG", help="benchmark compilation flags")
    args = parser.parse_args()
    if (args.release_source is None) != (args.release_build is None):
        parser.error("--release-source and --release-build must be provided together")
    return args


def makefile_variable(makefile: Path, name: str) -> str:
    """Read one configured Makefile assignment, including Automake continuation lines."""
    lines = makefile.read_text().splitlines()
    prefix = f"{name} ="
    for position, line in enumerate(lines):
        if not line.startswith(prefix):
            continue
        value = line[len(prefix):].strip()
        while value.endswith("\\"):
            value = value[:-1].rstrip() + " "
            position += 1
            if position >= len(lines):
                raise RuntimeError(
                    f"{name} in {makefile} ends with a dangling continuation backslash"
                )
            value += lines[position].strip()
        return value
    raise RuntimeError(f"unable to find {name} in {makefile}")


def configured_cppflags(source_root: Path, build_root: Path) -> list[str]:
    """Reuse configured third-party include paths while replacing Make variables with absolute roots."""
    makefile = build_root / "ompi" / "test" / "datatype" / "Makefile"
    if not makefile.is_file():
        raise RuntimeError(f"configured datatype Makefile does not exist: {makefile}")
    value = makefile_variable(makefile, "CPPFLAGS")
    replacements = {
        "$(top_srcdir)": str(source_root),
        "${top_srcdir}": str(source_root),
        "$(top_builddir)": str(build_root),
        "${top_builddir}": str(build_root),
    }
    for old, new in replacements.items():
        value = value.replace(old, new)
    unresolved = [token for token in shlex.split(value) if "$(" in token or "${" in token]
    if unresolved:
        raise RuntimeError(f"unresolved Make variables in CPPFLAGS: {' '.join(unresolved)}")
    return shlex.split(value)


def main() -> int:
    args = parse_args()
    source = args.source.expanduser().resolve()
    source_root = args.release_source.expanduser().resolve() if args.release_source is not None else None
    build_root = args.release_build.expanduser().resolve() if args.release_build is not None else None
    # Open MPI's wrapper selects its configuration from argv[0], so retain the mpicc symlink name.
    mpicc = args.mpicc.expanduser().absolute()
    output = args.output.expanduser().resolve()
    for label, path in (("source", source), ("mpicc", mpicc)):
        if not path.is_file():
            raise RuntimeError(f"{label} does not exist: {path}")
    include_flags: list[str] = []
    configured_flags: list[str] = []
    if source_root is not None and build_root is not None:
        for label, path in (("release source", source_root), ("release build", build_root)):
            if not path.is_dir():
                raise RuntimeError(f"{label} does not exist: {path}")
        include_flags = [
            f"-iquote{source_root}", f"-iquote{build_root}",
            f"-iquote{source_root / 'opal' / 'include'}",
            f"-iquote{source_root / 'ompi' / 'include'}",
            f"-iquote{source_root / 'oshmem' / 'include'}",
            f"-I{build_root / 'opal' / 'include'}", f"-I{build_root / 'ompi' / 'include'}",
            f"-I{build_root / 'oshmem' / 'include'}",
            f"-I{build_root / 'ompi' / 'mpiext' / 'cuda' / 'c'}",
            f"-I{build_root / 'ompi' / 'mpiext' / 'rocm' / 'c'}",
        ]
        configured_flags = configured_cppflags(source_root, build_root)
    command = [
        str(mpicc), "-std=gnu11", *shlex.split(args.cflags), *include_flags,
        *configured_flags, str(source), "-lm", "-o", str(output),
    ]
    output.parent.mkdir(parents=True, exist_ok=True)
    result = subprocess.run(command, text=True, capture_output=True)
    metadata = {
        "command": command,
        "cwd": os.getcwd(),
        "release_source": str(source_root) if source_root is not None else None,
        "release_build": str(build_root) if build_root is not None else None,
        "mpicc": str(mpicc),
        "source": str(source),
        "returncode": result.returncode,
        "stdout": result.stdout,
        "stderr": result.stderr,
    }
    Path(f"{output}.build.json").write_text(json.dumps(metadata, indent=2) + "\n")
    if 0 != result.returncode:
        raise RuntimeError(f"compilation failed; see {output}.build.json")

    environment = os.environ.copy()
    environment["OMPI_MCA_btl"] = "self"
    validation = subprocess.run([str(output), "--help"], env=environment, text=True,
                                capture_output=True)
    if 0 != validation.returncode or "--min-work-bytes" not in validation.stdout:
        raise RuntimeError(f"built executable failed CLI validation: {output}")
    print(output)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
