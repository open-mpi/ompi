#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

"""Compare one base to_self pack or unpack implementation with other MPI installations."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import os
import platform
import random
import re
import shlex
import statistics
import subprocess
import sys
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from datatype_bench_common import cpu_description, sha256  # noqa: E402


DATA_RE = re.compile(r"(?:^|,\s*)(\d+)=([a-z0-9_]+)")
RESULT_RE = re.compile(
    r"^\s*(\d+)\s+(\S+)\s+(\S+)\s+MB/s\s+"
    r"\[min\s+(\S+)\s+max\s+(\S+)\s+std\s+(\S+)%\]$"
)
RAW_TIMER_RE = re.compile(
    r"^# raw-timer\t([a-z0-9_]+)\t([a-z0-9_]+)\t(\d+)\t(\d+)\t(\S+)\t([01])$"
)
# to_self --validate emits one such comment per datatype; PASS/FAIL/SKIP per direction.
VALIDATION_RE = re.compile(
    r"^# VALIDATION\s+([a-z0-9_]+)\s+pack=(PASS|FAIL|SKIP)\s+unpack=(PASS|FAIL|SKIP)$"
)
BOOTSTRAP_SAMPLES = 10000
COMMUNICATION_OPERATIONS = ("isend_recv", "isend_irecv", "irecv_send", "irecv_isend")


@dataclass(frozen=True)
class Implementation:
    """One benchmark executable, matching launcher, result key, label, and operation."""

    key: str
    label: str
    executable: Path
    launcher: Path | None
    operation: str


def parse_args() -> argparse.Namespace:
    directory = Path(__file__).resolve().parent
    repository = directory.parents[1]
    parser = argparse.ArgumentParser(
        description="Compare a base MPI pack or unpack implementation with other MPIs.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "example:\n"
            "  run_to_self_pack_comparison.py --base current_dir \\\n"
            "      --against MPICH mpich_dir --against 'Open MPI 5.x' ompi5_dir \\\n"
            "      --output results"
        ),
    )
    parser.add_argument(
        "--base", type=Path,
        help="base to_self executable, build directory, mpicc, or MPI installation prefix",
    )
    parser.add_argument("--base-label", default="current", help="base display label")
    parser.add_argument(
        "--operation", choices=("pack", "unpack", *COMMUNICATION_OPERATIONS), default="pack"
    )
    parser.add_argument("--send", choices=("ddt", "packed"), default="ddt")
    parser.add_argument("--recv", choices=("ddt", "packed"), default="ddt")
    parser.add_argument(
        "--against", nargs=2, action="append", default=[], metavar=("LABEL", "PATH"),
        help="comparison label and to_self executable, build directory, mpicc, or MPI prefix",
    )
    parser.add_argument(
        "--source", type=Path, default=repository / "ompi" / "test" / "datatype" / "to_self.c",
        help="public MPI benchmark source used when an installation prefix is provided",
    )
    parser.add_argument("--cflags", default="-O3 -DNDEBUG", help="flags used to compile to_self")
    # Preserve commands recorded by the original two-implementation runner.
    parser.add_argument("--current", type=Path, help=argparse.SUPPRESS)
    parser.add_argument("--ompi5", type=Path, help=argparse.SUPPRESS)
    parser.add_argument("--current-label", default="current", help=argparse.SUPPRESS)
    parser.add_argument("--ompi5-label", default="Open MPI 5.x", help=argparse.SUPPRESS)
    parser.add_argument("--output", type=Path, required=True, help="result directory")
    parser.add_argument("--data", default="all", help="comma-separated datatype names or indexes")
    parser.add_argument("--cycles", type=int, default=100)
    parser.add_argument("--trials", type=int, default=40)
    parser.add_argument("--warmups", type=int, default=5)
    parser.add_argument("--min-work-bytes", type=int, default=1024 * 1024)
    parser.add_argument("--repetitions", type=int, default=5)
    parser.add_argument(
        "--preserve-full-order", action="store_true",
        help="run preceding datatypes as context before selected datatype measurements",
    )
    parser.add_argument(
        "--command-prefix", default="",
        help="command placed before each executable, for example 'taskset -c 3'",
    )
    parser.add_argument(
        "--launcher-arg", action="append", default=[],
        help="argument placed after each matching mpirun; repeat for multiple arguments",
    )
    parser.add_argument("--force", action="store_true", help="overwrite existing raw measurements")
    parser.add_argument(
        "--exclude-hand-made", action="store_true",
        help="skip the base implementation versus hand-made comparison",
    )
    parser.add_argument("--no-plot", action="store_true", help="collect data without plotting")
    parser.add_argument(
        "--plotter", type=Path, default=directory / "plot_to_self_pack_comparison.py"
    )
    args = parser.parse_args()

    for name in ("cycles", "trials", "repetitions"):
        if getattr(args, name) < 1:
            parser.error(f"--{name} must be positive")
    if args.trials < 5:
        parser.error("--trials must be at least 5 for outlier filtering")
    if args.warmups < 0:
        parser.error("--warmups cannot be negative")
    if args.min_work_bytes < 0:
        parser.error("--min-work-bytes cannot be negative")
    if args.operation in COMMUNICATION_OPERATIONS:
        if "packed" == args.send and "packed" == args.recv:
            parser.error("communication requires at least one derived-datatype endpoint")
        args.processes = 2
        args.scenario = f"{args.operation}_{args.send}_to_{args.recv}"
        args.exclude_hand_made = True
    else:
        if "ddt" != args.send or "ddt" != args.recv:
            parser.error("--send and --recv apply only to communication operations")
        args.processes = 1
        args.scenario = args.operation
    if args.base is None:
        if args.current is None or args.ompi5 is None:
            parser.error("--base is required")
        if args.against:
            parser.error("--against cannot be combined with --current/--ompi5")
        args.base = args.current
        args.base_label = args.current_label
        args.against = [(args.ompi5_label, str(args.ompi5))]
    elif args.current is not None or args.ompi5 is not None:
        parser.error("--base/--against cannot be combined with --current/--ompi5")

    if args.exclude_hand_made and not args.against:
        parser.error("--exclude-hand-made requires at least one --against comparison")
    labels = [args.base_label, *(label for label, _ in args.against)]
    if not args.exclude_hand_made:
        labels.append("hand-made")
    if any(not label.strip() or "\t" in label or "\n" in label for label in labels):
        parser.error("implementation labels must be nonempty single-line strings without tabs")
    if len(labels) != len(set(labels)):
        parser.error("implementation labels must be unique")
    return args


def compile_benchmark(
    args: argparse.Namespace, key: str, label: str, mpicc: Path
) -> Path:
    """Compile the public MPI benchmark with one installation's compiler wrapper."""
    source = args.source.expanduser().resolve()
    if not source.is_file():
        raise RuntimeError(f"benchmark source does not exist: {source}")

    output = args.output / "binaries" / key / "to_self"
    output.parent.mkdir(parents=True, exist_ok=True)
    metadata_path = Path(f"{output}.build.json")
    build_identity = {
        "label": label,
        "source": str(source),
        "source_sha256": sha256(source),
        "mpicc": str(mpicc),
        "mpicc_sha256": sha256(mpicc),
        "cflags": args.cflags,
    }
    if not args.force and output.is_file() and metadata_path.is_file():
        previous = json.loads(metadata_path.read_text())
        identity_matches = all(
            previous.get(name) == value for name, value in build_identity.items()
        )
        if 0 == previous.get("returncode") and identity_matches:
            return output.resolve()
        if 0 == previous.get("returncode"):
            # A successful build for a *different* configuration lives here; do
            # not silently overwrite it.
            raise RuntimeError(
                f"existing build configuration differs: {metadata_path}; "
                "use --force or a new output directory"
            )
    # A missing binary or a recorded failed build (metadata but no usable
    # to_self) falls through and is rebuilt in place.  --force is deliberately
    # not required here: it would also discard every cached measurement .out.
    command = [
        str(mpicc), "-std=gnu11", *shlex.split(args.cflags), str(source), "-lm", "-o", str(output),
    ]
    result = subprocess.run(command, text=True, capture_output=True)
    metadata = build_identity | {
        "command": command,
        "returncode": result.returncode,
        "stdout": result.stdout,
        "stderr": result.stderr,
    }
    metadata_path.write_text(json.dumps(metadata, indent=2) + "\n")
    if 0 != result.returncode:
        raise RuntimeError(f"unable to build {label}; see {output}.build.json")
    return output.resolve()


def resolve_launcher(mpicc: Path) -> Path | None:
    """Find the mpirun or mpiexec installed beside one compiler wrapper.

    Debian/Fedora "alternatives" layouts keep several MPIs in a single bin/ with
    suffixed wrappers (mpicc.mpich, mpicc.openmpi) while the unsuffixed mpirun is
    a symlink to whichever MPI is currently selected.  When the wrapper carries a
    suffix, prefer the matching suffixed launcher so a comparison never launches
    a different MPI than the one that compiled the benchmark.
    """
    _, dot, suffix = mpicc.name.partition(".")
    for base in ("mpirun", "mpiexec"):
        names = (f"{base}.{suffix}", base) if dot else (base,)
        for name in names:
            candidate = mpicc.parent / name
            if candidate.is_file() and os.access(candidate, os.X_OK):
                return candidate.resolve()
    return None


def build_identity(implementation: Implementation) -> str:
    """Identify the MPI under test by the version string compiled into its library.

    The benchmark binary is frequently a libtool wrapper script whose contents never change
    across rebuilds, and the pack/unpack code under test lives in the separately loaded
    libmpi/libopen-pal -- so hashing anything on disk is a poor rebuild detector.  Instead ask the
    benchmark to print MPI_Get_library_version(), which reports the repo revision compiled into the
    actually-linked library (and works for any MPI-3 implementation).  When the binary predates the
    --library-version flag we cannot identify the build, so fall back to the wall-clock time: that
    makes every run compare as a distinct build and keeps cached measurements from being blended.
    """
    result = subprocess.run(
        [str(implementation.executable), "--library-version"],
        env=benchmark_environment(1), text=True, capture_output=True,
    )
    identity = result.stdout.strip()
    if 0 == result.returncode and identity:
        return identity
    return f"unidentified build at {datetime.now(timezone.utc).isoformat()}"


def to_self_supports_cli(executable: Path) -> bool:
    """Report whether a prebuilt to_self understands the options this tool needs."""
    result = subprocess.run(
        [str(executable), "--help"], env=benchmark_environment(1), text=True,
        capture_output=True,
    )
    help_text = result.stdout + result.stderr
    return all(option in help_text for option in ("--data", "--trials", "--raw-timers"))


def resolve_benchmark(
    args: argparse.Namespace, key: str, label: str, requested: Path
) -> tuple[Path, Path | None]:
    """Resolve an existing tester or compile one from an MPI compiler wrapper or prefix."""
    path = requested.expanduser().absolute()
    if path.is_file():
        if path.name.startswith("mpicc"):
            return compile_benchmark(args, key, label, path), resolve_launcher(path)
        if not os.access(path, os.X_OK):
            raise RuntimeError(f"{label} benchmark is not executable: {path}")
        return path.resolve(), None
    if not path.is_dir():
        raise RuntimeError(f"{label} path does not exist: {path}")

    stale_to_self: Path | None = None
    for relative in (
        "to_self",
        "ompi/test/datatype/to_self",
        "ompi/test/datatype/.libs/to_self",
        "test/datatype/to_self",
        "test/datatype/.libs/to_self",
    ):
        candidate = path / relative
        if candidate.is_file() and os.access(candidate, os.X_OK):
            if not to_self_supports_cli(candidate):
                # Too old to accept --data/--trials/--raw-timers; prefer recompiling
                # from a compiler wrapper in the same tree over failing outright.
                stale_to_self = candidate
                break
            launcher = next(
                (
                    item.resolve()
                    for item in (path / "bin" / "mpirun", path / "bin" / "mpiexec")
                    if item.is_file() and os.access(item, os.X_OK)
                ),
                None,
            )
            return candidate.resolve(), launcher
    for relative in ("bin/mpicc", "bin/mpicc.mpich", "bin/mpicc.openmpi", "mpicc"):
        candidate = path / relative
        if candidate.is_file() and os.access(candidate, os.X_OK):
            return compile_benchmark(args, key, label, candidate.absolute()), resolve_launcher(
                candidate.absolute()
            )
    if stale_to_self is not None:
        raise RuntimeError(
            f"{label}: {stale_to_self} predates the options this comparison requires "
            "(--data/--trials/--raw-timers) and no mpicc wrapper was found beside it to "
            "recompile the current benchmark source; provide that MPI's mpicc instead"
        )
    raise RuntimeError(
        f"{label} path contains neither to_self nor an MPI compiler wrapper: {path}"
    )


def resolve_implementations(args: argparse.Namespace) -> tuple[Implementation, ...]:
    """Resolve the base, requested comparison MPIs, and optional hand operation."""
    base_executable, base_launcher = resolve_benchmark(args, "base", args.base_label, args.base)
    base = Implementation(
        "base", args.base_label, base_executable, base_launcher, args.operation,
    )
    against_list = []
    for position, (label, path) in enumerate(args.against, start=1):
        key = f"against_{position:02d}"
        executable, launcher = resolve_benchmark(args, key, label, Path(path))
        against_list.append(Implementation(key, label, executable, launcher, args.operation))
    against = tuple(against_list)
    hand = () if args.exclude_hand_made else (
        Implementation(
            "hand", "hand-made", base.executable, base.launcher, f"{args.operation}_byhand"
        ),
    )
    if args.processes > 1:
        missing = [implementation.label for implementation in (base, *against) if implementation.launcher is None]
        if missing:
            raise RuntimeError(
                "two-rank comparisons require an MPI prefix or mpicc with a matching launcher: "
                + ", ".join(missing)
            )
    return (base, *hand, *against)


def comparison_pairs(
    implementations: tuple[Implementation, ...]
) -> tuple[tuple[Implementation, Implementation], ...]:
    """Compare the base with optional hand code and requested implementations with the base."""
    base = implementations[0]
    return tuple(
        (base, implementation) if "hand" == implementation.key else (implementation, base)
        for implementation in implementations[1:]
    )


def benchmark_environment(processes: int) -> dict[str, str]:
    """Restrict singleton Open MPI jobs to self while leaving two-rank transports available."""
    environment = os.environ.copy()
    if 1 == processes:
        environment["OMPI_MCA_btl"] = "self"
    else:
        environment.pop("OMPI_MCA_btl", None)
    return environment


def implementation_command(
    implementation: Implementation, processes: int, command_prefix: list[str],
    launcher_args: list[str],
) -> list[str]:
    """Build a command with the implementation's own launcher."""
    if implementation.launcher is not None:
        return command_prefix + [
            str(implementation.launcher), "-n", str(processes), *launcher_args,
            str(implementation.executable),
        ]
    if 1 != processes:
        raise RuntimeError(f"{implementation.label} has no launcher for a {processes}-rank run")
    return command_prefix + [str(implementation.executable)]


def discover_datatypes(
    implementation: Implementation, command_prefix: list[str], launcher_args: list[str]
) -> list[tuple[int, str]]:
    """Read the indexed --data namespace from to_self itself to avoid a duplicated test list."""
    command = implementation_command(implementation, 1, command_prefix, launcher_args)
    result = subprocess.run(
        command + ["--help"], env=benchmark_environment(1), text=True, capture_output=True,
    )
    if 0 != result.returncode:
        raise RuntimeError(
            f"unable to query {implementation.executable}:\n{result.stdout}{result.stderr}"
        )
    datatypes = [(int(index), name) for index, name in DATA_RE.findall(result.stdout)]
    if not datatypes:
        raise RuntimeError(f"unable to parse datatype list from {implementation.executable}")
    return datatypes


def select_datatypes(
    requested: str, base: list[tuple[int, str]], comparisons: dict[str, list[tuple[int, str]]]
) -> list[tuple[int, str]]:
    """Select by base index or name and require matching names in every comparison tester."""
    base_by_name = {name: index for index, name in base}
    base_by_index = dict(base)
    if "all" == requested.strip().lower():
        selected = base
    else:
        selected = []
        for token in requested.split(","):
            token = token.strip()
            if token.isdigit() and int(token) in base_by_index:
                item = (int(token), base_by_index[int(token)])
            elif token in base_by_name:
                item = (base_by_name[token], token)
            else:
                raise RuntimeError(f"unknown datatype selection: {token}")
            if item not in selected:
                selected.append(item)
    for label, available in comparisons.items():
        available_names = {name for _, name in available}
        missing = [name for _, name in selected if name not in available_names]
        if missing:
            raise RuntimeError(f"{label} lacks selected datatypes: {', '.join(missing)}")
    return selected


def parse_validation_status(text: str, operation: str) -> str:
    """Reduce to_self's per-datatype '# VALIDATION' line to the measured direction.

    Returns PASS/FAIL/SKIP for the direction the operation exercises, or UNKNOWN
    when the run carried no validation line (an executable too old for --validate,
    or a run that did not request it). Communication operations rely on both
    directions, so they take the worse of the two (FAIL dominates, then SKIP).
    """
    pack = unpack = None
    for line in text.splitlines():
        match = VALIDATION_RE.match(line)
        if match:
            pack, unpack = match.group(2), match.group(3)
            break
    if pack is None:
        return "UNKNOWN"
    if operation.startswith("unpack"):
        return unpack
    if operation.startswith("pack"):
        return pack
    # Communication moves ddt -> packed -> ddt and needs both engines correct.
    for verdict in ("FAIL", "SKIP"):
        if verdict in (pack, unpack):
            return verdict
    return "PASS"


def parse_measurements(
    text: str, operation: str, expected_trials: int,
) -> tuple[
    list[tuple[int, float, float, float, float, float]],
    list[tuple[str, int, int, float, bool]],
]:
    """Extract summaries and require the complete raw trial stream from one run."""
    measurements = []
    raw_timers = []
    for line in text.splitlines():
        match = RESULT_RE.match(line)
        if match:
            measurements.append(
                (
                    int(match.group(1)), float(match.group(2)), float(match.group(3)),
                    float(match.group(4)), float(match.group(5)), float(match.group(6)),
                )
            )
            continue
        match = RAW_TIMER_RE.match(line)
        if match:
            # Groups: 1=datatype, 2=operation, 3=length, 4=trial, 5=seconds, 6=retained.
            # The datatype disambiguates rows when a run emits more than one datatype;
            # this single-datatype consumer keeps the historical (operation, ...) tuple.
            raw_timers.append(
                (
                    match.group(2), int(match.group(3)), int(match.group(4)),
                    float(match.group(5)), bool(int(match.group(6))),
                )
            )
    if not measurements:
        raise RuntimeError("benchmark output did not contain timing rows")
    if any(not math.isfinite(row[1]) or row[1] <= 0.0 for row in measurements):
        raise RuntimeError("benchmark produced zero or non-finite timings; increase --cycles")
    sizes = [row[0] for row in measurements]
    if len(sizes) != len(set(sizes)):
        raise RuntimeError("benchmark output contains more than one operation section")
    if any(raw_operation != operation for raw_operation, *_ in raw_timers):
        raise RuntimeError(f"benchmark raw timers do not match requested operation {operation}")
    for size in sizes:
        size_timers = [row for row in raw_timers if row[1] == size]
        trial_indexes = sorted(row[2] for row in size_timers)
        if trial_indexes != list(range(expected_trials)):
            raise RuntimeError(
                f"benchmark output has incomplete raw timers for {operation} at {size} bytes"
            )
        if any(not math.isfinite(row[3]) or row[3] <= 0.0 for row in size_timers):
            raise RuntimeError(f"benchmark produced invalid raw timers at {size} bytes")
    if len(raw_timers) != len(sizes) * expected_trials:
        raise RuntimeError("benchmark output contains unexpected raw timer rows")
    return measurements, raw_timers


def write_run_config(
    args: argparse.Namespace, implementations: tuple[Implementation, ...]
) -> None:
    """Refuse to resume into a directory created with incompatible benchmark controls."""
    config_path = args.output / "run_config.json"
    config = {
        # Host identity is part of the resume contract: reusing cached .out files
        # from another machine would silently blend measurements from two hosts.
        "hostname": platform.node(),
        "machine": platform.machine(),
        "implementations": [
            {
                "key": implementation.key,
                "label": implementation.label,
                "executable": str(implementation.executable),
                "library_version": build_identity(implementation),
                "launcher": str(implementation.launcher) if implementation.launcher else None,
                "operation": implementation.operation,
            }
            for implementation in implementations
        ],
        "cycles": args.cycles,
        "trials": args.trials,
        "warmups": args.warmups,
        "min_work_bytes": args.min_work_bytes,
        "repetitions": args.repetitions,
        "data": args.data,
        "scenario": args.scenario,
        "send": args.send,
        "recv": args.recv,
        "processes": args.processes,
        "preserve_full_order": args.preserve_full_order,
        "raw_timers": True,
        "command_prefix": args.command_prefix,
        "launcher_args": args.launcher_arg,
    }
    if config_path.exists() and not args.force:
        previous = json.loads(config_path.read_text())
        # Only keys present in both configs need to agree.  Comparing just the
        # intersection lets a directory written by an older version (which may
        # lack keys added since, such as hostname/machine) still resume when
        # every shared setting is unchanged, while a genuine conflict (including
        # a host change once both configs record it) still fails.
        differing = sorted(
            key for key in previous.keys() & config.keys() if previous[key] != config[key]
        )
        if differing:
            raise RuntimeError(
                f"existing configuration differs ({', '.join(differing)}): {config_path}; "
                "use --force or a new output directory"
            )
    config_path.write_text(json.dumps(config, indent=2, sort_keys=True) + "\n")


def write_manifest(
    args: argparse.Namespace, implementations: tuple[Implementation, ...]
) -> None:
    """Record enough machine and binary identity to compare results across architectures."""
    lines = [
        f"timestamp_utc={datetime.now(timezone.utc).isoformat()}",
        f"hostname={platform.node()}",
        f"platform={platform.platform()}",
        f"machine={platform.machine()}",
        f"processor={cpu_description()}",
        f"python={platform.python_version()}",
    ]
    for implementation in implementations:
        lines.extend(
            (
                f"{implementation.key}_label={implementation.label}",
                f"{implementation.key}_operation={implementation.operation}",
                f"{implementation.key}_executable={implementation.executable}",
                f"{implementation.key}_library_version={build_identity(implementation)}",
                f"{implementation.key}_launcher={implementation.launcher or 'none'}",
            )
        )
    (args.output / "manifest.txt").write_text("\n".join(lines) + "\n")


def run_measurement(
    args: argparse.Namespace, implementation: Implementation, command_prefix: list[str],
    datatype: str, repetition: int, output: Path, error: Path, command_log, measured: bool,
) -> tuple[
    list[tuple[int, float, float, float, float, float]],
    list[tuple[str, int, int, float, bool]],
    str,
]:
    """Run one implementation in a fresh process or validate and reuse its saved output.

    Also returns the correctness self-check verdict (PASS/FAIL/SKIP/UNKNOWN) for
    the measured direction, so a fast-but-wrong result cannot pass as a speedup.
    """
    command = implementation_command(
        implementation, args.processes, command_prefix, args.launcher_arg
    ) + [
        f"--data={datatype}",
        f"--check={implementation.operation}",
        f"--cycles={args.cycles}", f"--trials={args.trials}", f"--warmups={args.warmups}",
        f"--min-work-bytes={args.min_work_bytes}", "--raw-timers", "--validate",
    ]
    if implementation.operation in COMMUNICATION_OPERATIONS:
        command.extend((f"--send={args.send}", f"--recv={args.recv}"))
    command_log.write(
        f"{datatype}\t{implementation.key}\t{implementation.label}\t{repetition}\t"
        f"{int(measured)}\t"
        f"{shlex.join(command)}\n"
    )
    command_log.flush()
    if output.exists() and not args.force:
        text = output.read_text(errors="replace")
        measurements, raw_timers = parse_measurements(
            text, implementation.operation, args.trials
        )
        return measurements, raw_timers, parse_validation_status(text, implementation.operation)
    result = subprocess.run(
        command, env=benchmark_environment(args.processes), text=True, capture_output=True
    )
    if 0 != result.returncode:
        # Keep stderr for debugging, but never write the reusable .out: on a
        # later run "--force"-less runs treat an existing .out as a valid cached
        # result, so a failed run must not leave one behind.
        error.write_text(result.stderr)
        raise RuntimeError(f"benchmark failed ({result.returncode}): {shlex.join(command)}")
    output.write_text(result.stdout)
    error.write_text(result.stderr)
    measurements, raw_timers = parse_measurements(
        result.stdout, implementation.operation, args.trials
    )
    return measurements, raw_timers, parse_validation_status(result.stdout, implementation.operation)


def bootstrap_median_ci(values: list[float], identity: str) -> tuple[float, float]:
    """Return a deterministic percentile-bootstrap 95% interval for a paired median."""
    if 1 == len(values):
        return values[0], values[0]
    seed = int.from_bytes(hashlib.sha256(identity.encode()).digest()[:8], "big")
    generator = random.Random(seed)
    medians = sorted(
        statistics.median(generator.choices(values, k=len(values)))
        for _ in range(BOOTSTRAP_SAMPLES)
    )
    return medians[int(0.025 * (BOOTSTRAP_SAMPLES - 1))], medians[int(0.975 * (BOOTSTRAP_SAMPLES - 1))]


def write_tables(
    args: argparse.Namespace, datatypes: list[tuple[int, str]],
    implementations: tuple[Implementation, ...], rows: list[dict[str, object]],
    trial_rows: list[dict[str, object]],
) -> Path:
    """Write every trial and paired filtered and unfiltered comparison statistics."""
    measurements_path = args.output / "measurements.tsv"
    fields = [
        "datatype_index", "datatype", "implementation_key", "implementation_label", "operation",
        "repetition", "size_bytes", "seconds", "unfiltered_seconds", "reported_seconds",
        "retained_trials", "bandwidth_mib_s", "min_seconds", "max_seconds", "stddev_pct",
        "raw_file", "valid",
    ]
    with measurements_path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields, delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)

    trials_path = args.output / "trials.tsv"
    trial_fields = [
        "datatype_index", "datatype", "implementation_key", "implementation_label", "operation",
        "repetition", "size_bytes", "trial", "seconds", "retained", "raw_file",
    ]
    with trials_path.open("w", newline="") as stream:
        writer = csv.DictWriter(
            stream, fieldnames=trial_fields, delimiter="\t", lineterminator="\n"
        )
        writer.writeheader()
        writer.writerows(trial_rows)

    grouped: dict[tuple[int, str, int, str, int], dict[str, object]] = {}
    for row in rows:
        key = (
            int(row["datatype_index"]), str(row["datatype"]), int(row["size_bytes"]),
            str(row["implementation_key"]), int(row["repetition"]),
        )
        if key in grouped:
            raise RuntimeError(f"duplicate measurement row: {key}")
        grouped[key] = row

    summary_path = args.output / "summary.tsv"
    summary_fields = [
        "datatype_index", "datatype", "operation", "size_bytes", "statistic", "comparison_key",
        "comparison_label", "subject_key", "subject_label", "subject_seconds", "baseline_key",
        "baseline_label", "baseline_seconds", "independent_speedup_pct", "speedup_pct",
        "ci_low_pct", "ci_high_pct", "mad_pct", "repetitions",
    ]
    summary_rows: list[dict[str, object]] = []
    pairs = comparison_pairs(implementations)
    with summary_path.open("w", newline="") as stream:
        writer = csv.DictWriter(
            stream, fieldnames=summary_fields, delimiter="\t", lineterminator="\n"
        )
        writer.writeheader()
        for index, datatype in datatypes:
            sizes = sorted(
                {size for group_index, group_name, size, _, _ in grouped
                 if group_index == index and group_name == datatype}
            )
            for size in sizes:
                if any(
                    (index, datatype, size, implementation.key, repetition) not in grouped
                    for implementation in implementations
                    for repetition in range(1, args.repetitions + 1)
                ):
                    raise RuntimeError(f"incomplete repetitions for {datatype} at {size} bytes")
                for statistic, field in (("filtered", "seconds"), ("unfiltered", "unfiltered_seconds")):
                    values = {
                        implementation.key: [
                            float(grouped[(index, datatype, size, implementation.key, repetition)][field])
                            for repetition in range(1, args.repetitions + 1)
                        ]
                        for implementation in implementations
                    }
                    medians = {key: statistics.median(samples) for key, samples in values.items()}
                    for subject, baseline in pairs:
                        paired = [
                            100.0 * (baseline_time / subject_time - 1.0)
                            for subject_time, baseline_time in zip(
                                values[subject.key], values[baseline.key]
                            )
                        ]
                        paired_median = statistics.median(paired)
                        mad = statistics.median(abs(value - paired_median) for value in paired)
                        comparison_key = f"{subject.key}_vs_{baseline.key}"
                        identity = f"{index}:{datatype}:{size}:{statistic}:{comparison_key}"
                        ci_low, ci_high = bootstrap_median_ci(paired, identity)
                        summary_row = {
                            "datatype_index": index,
                            "datatype": datatype,
                            "operation": args.scenario,
                            "size_bytes": size,
                            "statistic": statistic,
                            "comparison_key": comparison_key,
                            "comparison_label": f"{subject.label} vs {baseline.label}",
                            "subject_key": subject.key,
                            "subject_label": subject.label,
                            "subject_seconds": f"{medians[subject.key]:.12g}",
                            "baseline_key": baseline.key,
                            "baseline_label": baseline.label,
                            "baseline_seconds": f"{medians[baseline.key]:.12g}",
                            "independent_speedup_pct":
                                f"{100.0 * (medians[baseline.key] / medians[subject.key] - 1.0):.6f}",
                            "speedup_pct": f"{paired_median:.6f}",
                            "ci_low_pct": f"{ci_low:.6f}",
                            "ci_high_pct": f"{ci_high:.6f}",
                            "mad_pct": f"{mad:.6f}",
                            "repetitions": args.repetitions,
                        }
                        summary_rows.append(summary_row)
                        writer.writerow(summary_row)

    aggregate_fields = [
        "datatype_index", "datatype", "operation", "statistic", "comparison_key", "comparison_label",
        "size_points", "geomean_pct", "worst_pct", "best_pct", "faster_points",
        "slower_points", "noise_points",
    ]
    aggregate_path = args.output / "datatype_summary.tsv"
    with aggregate_path.open("w", newline="") as stream:
        writer = csv.DictWriter(
            stream, fieldnames=aggregate_fields, delimiter="\t", lineterminator="\n"
        )
        writer.writeheader()
        for index, datatype in datatypes:
            for statistic in ("filtered", "unfiltered"):
                for subject, baseline in pairs:
                    comparison_key = f"{subject.key}_vs_{baseline.key}"
                    comparison_label = f"{subject.label} vs {baseline.label}"
                    comparison_rows = [
                        row for row in summary_rows
                        if int(row["datatype_index"]) == index and row["datatype"] == datatype
                        and row["statistic"] == statistic
                        and row["comparison_key"] == comparison_key
                    ]
                    values = [float(row["speedup_pct"]) for row in comparison_rows]
                    ratios = [1.0 + value / 100.0 for value in values]
                    writer.writerow(
                        {
                            "datatype_index": index,
                            "datatype": datatype,
                            "operation": args.scenario,
                            "statistic": statistic,
                            "comparison_key": comparison_key,
                            "comparison_label": comparison_label,
                            "size_points": len(comparison_rows),
                            "geomean_pct": f"{100.0 * (math.exp(statistics.fmean(map(math.log, ratios))) - 1.0):.6f}",
                            "worst_pct": f"{min(values):.6f}",
                            "best_pct": f"{max(values):.6f}",
                            "faster_points": sum(value > 3.0 for value in values),
                            "slower_points": sum(value < -3.0 for value in values),
                            "noise_points": sum(-3.0 <= value <= 3.0 for value in values),
                        }
                    )
    return summary_path


def main() -> int:
    args = parse_args()
    args.output = args.output.expanduser().resolve()
    args.output.mkdir(parents=True, exist_ok=True)
    command_prefix = shlex.split(args.command_prefix)

    implementations = resolve_implementations(args)
    write_run_config(args, implementations)
    write_manifest(args, implementations)
    base_datatypes = discover_datatypes(
        implementations[0], command_prefix, args.launcher_arg
    )
    comparison_datatypes = {
        implementation.label: discover_datatypes(
            implementation, command_prefix, args.launcher_arg
        )
        for implementation in implementations if implementation.key.startswith("against_")
    }
    datatypes = select_datatypes(args.data, base_datatypes, comparison_datatypes)
    full_positions = {name: position for position, (_, name) in enumerate(base_datatypes)}
    if args.preserve_full_order:
        last_position = max(full_positions[name] for _, name in datatypes)
        execution_datatypes = base_datatypes[:last_position + 1]
    else:
        execution_datatypes = datatypes
    for label, available in comparison_datatypes.items():
        available_names = {name for _, name in available}
        missing = [name for _, name in execution_datatypes if name not in available_names]
        if missing:
            raise RuntimeError(f"{label} lacks context datatypes: {', '.join(missing)}")

    rows: list[dict[str, object]] = []
    trial_rows: list[dict[str, object]] = []
    # Worst-seen correctness verdict per (datatype, implementation label). FAIL
    # dominates so a single wrong repetition cannot be hidden by later PASSes.
    validation_status: dict[tuple[str, str], str] = {}
    validation_severity = {"FAIL": 3, "UNKNOWN": 2, "SKIP": 1, "PASS": 0}
    selected = set(datatypes)
    raw_root = args.output / "raw"
    raw_root.mkdir(exist_ok=True)
    context_root = args.output / "context_raw"
    if args.preserve_full_order:
        context_root.mkdir(exist_ok=True)
    with (args.output / "commands.tsv").open("w") as command_log:
        command_log.write(
            "datatype\timplementation_key\timplementation_label\trepetition\tmeasured\tcommand\n"
        )
        for execution_position, (index, datatype) in enumerate(execution_datatypes):
            measured = (index, datatype) in selected
            datatype_dir = (raw_root if measured else context_root) / f"{index:02d}_{datatype}"
            datatype_dir.mkdir(exist_ok=True)
            kind = "measure" if measured else "context"
            print(
                f"[{execution_position + 1}/{len(execution_datatypes)}] "
                f"{index}={datatype} ({kind})",
                flush=True,
            )
            expected_sizes: list[int] | None = None
            for repetition in range(1, args.repetitions + 1):
                shift = (full_positions[datatype] + repetition - 1) % len(implementations)
                order = implementations[shift:] + implementations[:shift]
                for implementation in order:
                    output = datatype_dir / f"{implementation.key}_r{repetition:02d}.out"
                    error = datatype_dir / f"{implementation.key}_r{repetition:02d}.err"
                    measurements, raw_timers, validation = run_measurement(
                        args, implementation, command_prefix, datatype, repetition, output, error,
                        command_log, measured,
                    )
                    if measured:
                        status_key = (datatype, implementation.label)
                        previous = validation_status.get(status_key)
                        if (previous is None
                                or validation_severity[validation]
                                > validation_severity[previous]):
                            validation_status[status_key] = validation
                            if "FAIL" == validation:
                                print(
                                    f"  !! VALIDATION FAILED: {implementation.label} produced "
                                    f"incorrect data for {datatype} ({implementation.operation}); "
                                    "its timings do not reflect a correct transfer",
                                    flush=True,
                                )
                    sizes = [measurement[0] for measurement in measurements]
                    if expected_sizes is None:
                        expected_sizes = sizes
                    elif sizes != expected_sizes:
                        raise RuntimeError(
                            f"size spectrum differs for {datatype}, {implementation}, repetition {repetition}"
                        )
                    if not measured:
                        continue
                    timers_by_size = {
                        size: [timer for timer in raw_timers if timer[1] == size]
                        for size in sizes
                    }
                    for size, seconds, bandwidth, minimum, maximum, stddev in measurements:
                        size_timers = timers_by_size[size]
                        retained = [timer[3] for timer in size_timers if timer[4]]
                        filtered_seconds = statistics.fmean(retained)
                        unfiltered_seconds = statistics.fmean(timer[3] for timer in size_timers)
                        if not math.isclose(filtered_seconds, seconds, rel_tol=1.0e-5):
                            raise RuntimeError(
                                f"reported filtered timing differs from raw trials for "
                                f"{implementation.label} {datatype} at {size} bytes"
                            )
                        relative_raw_file = str(output.relative_to(args.output))
                        rows.append(
                            {
                                "datatype_index": index,
                                "datatype": datatype,
                                "implementation_key": implementation.key,
                                "implementation_label": implementation.label,
                                "operation": implementation.operation,
                                "repetition": repetition,
                                "size_bytes": size,
                                "seconds": f"{filtered_seconds:.17g}",
                                "unfiltered_seconds": f"{unfiltered_seconds:.17g}",
                                "reported_seconds": f"{seconds:.12g}",
                                "retained_trials": len(retained),
                                "bandwidth_mib_s": f"{bandwidth:.6f}",
                                "min_seconds": f"{minimum:.12g}",
                                "max_seconds": f"{maximum:.12g}",
                                "stddev_pct": f"{stddev:.6f}",
                                "raw_file": relative_raw_file,
                                "valid": validation,
                            }
                        )
                        for _, _, trial, trial_seconds, trial_retained in size_timers:
                            trial_rows.append(
                                {
                                    "datatype_index": index,
                                    "datatype": datatype,
                                    "implementation_key": implementation.key,
                                    "implementation_label": implementation.label,
                                    "operation": implementation.operation,
                                    "repetition": repetition,
                                    "size_bytes": size,
                                    "trial": trial,
                                    "seconds": f"{trial_seconds:.17g}",
                                    "retained": int(trial_retained),
                                    "raw_file": relative_raw_file,
                                }
                            )

    if validation_status:
        validation_path = args.output / "validation.tsv"
        with validation_path.open("w", newline="") as stream:
            writer = csv.writer(stream, delimiter="\t", lineterminator="\n")
            writer.writerow(("datatype", "implementation_label", "status"))
            for (datatype, label), status in sorted(validation_status.items()):
                writer.writerow((datatype, label, status))
        failed = sorted(
            (datatype, label)
            for (datatype, label), status in validation_status.items()
            if "FAIL" == status
        )
        if failed:
            print(
                "WARNING: correctness self-check FAILED for "
                f"{len(failed)} datatype/implementation pair(s); their timings do not "
                "reflect a correct pack/unpack and must not be read as speedups:",
                flush=True,
            )
            for datatype, label in failed:
                print(f"  - {label}: {datatype}", flush=True)

    summary = write_tables(args, datatypes, implementations, rows, trial_rows)
    if not args.no_plot:
        command = [
            sys.executable, str(args.plotter.expanduser().resolve()), "--input", str(summary),
            "--output", str(args.output / "graphs"),
        ]
        subprocess.run(command, check=True)
    print(f"Results: {args.output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, subprocess.SubprocessError, ValueError) as error:
        # ValueError covers json.JSONDecodeError from a truncated run_config.json /
        # build.json and statistics.StatisticsError from an all-filtered cached .out
        # on the resume path; without it those escape as a raw traceback.  This
        # mirrors the handler in run_to_self_suite_comparison.py.
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
