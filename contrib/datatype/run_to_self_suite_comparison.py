#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

"""Run the complete local pack, unpack, and communication comparison suite."""

from __future__ import annotations

import argparse
import csv
import json
import shlex
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path


SCENARIOS = (
    ("pack", "ddt", "ddt", "pack"),
    ("unpack", "ddt", "ddt", "unpack"),
    ("isend_recv", "ddt", "packed", "isend_recv_ddt_to_packed"),
    ("isend_recv", "packed", "ddt", "isend_recv_packed_to_ddt"),
    ("isend_recv", "ddt", "ddt", "isend_recv_ddt_to_ddt"),
)


def positive_int(value: str) -> int:
    """Parse a positive command-line integer."""
    parsed = int(value)
    if parsed < 1:
        raise argparse.ArgumentTypeError("value must be positive")
    return parsed


def nonnegative_int(value: str) -> int:
    """Parse a nonnegative command-line integer."""
    parsed = int(value)
    if parsed < 0:
        raise argparse.ArgumentTypeError("value cannot be negative")
    return parsed


def parse_args() -> argparse.Namespace:
    """Parse implementation paths and the effort shared by all five scenarios."""
    directory = Path(__file__).resolve().parent
    repository = directory.parents[1]
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "example:\n"
            "  run_to_self_suite_comparison.py --base-label MPICH --base /opt/mpich \\\n"
            "      --against current /opt/ompi-current \\\n"
            "      --against 'Open MPI 5.x' /opt/ompi-5 --output results"
        ),
    )
    parser.add_argument("--base", type=Path, required=True, help="baseline MPI prefix or mpicc")
    parser.add_argument("--base-label", default="MPICH")
    parser.add_argument(
        "--against", nargs=2, action="append", required=True, metavar=("LABEL", "PATH")
    )
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument(
        "--source", type=Path, default=repository / "ompi" / "test" / "datatype" / "to_self.c"
    )
    parser.add_argument("--cflags", default="-O3 -DNDEBUG")
    parser.add_argument("--data", default="all")
    parser.add_argument("--cycles", type=positive_int, default=100)
    parser.add_argument("--trials", type=positive_int, default=40)
    parser.add_argument("--warmups", type=nonnegative_int, default=5)
    parser.add_argument("--min-work-bytes", type=nonnegative_int, default=1024 * 1024)
    parser.add_argument("--communication-min-work-bytes", type=nonnegative_int, default=0)
    parser.add_argument("--repetitions", type=positive_int, default=5)
    parser.add_argument("--preserve-full-order", action="store_true")
    parser.add_argument("--command-prefix", default="")
    parser.add_argument("--launcher-arg", action="append", default=[])
    parser.add_argument("--force", action="store_true")
    parser.add_argument("--no-plot", action="store_true")
    parser.add_argument(
        "--runner", type=Path, default=directory / "run_to_self_pack_comparison.py"
    )
    parser.add_argument(
        "--plotter", type=Path, default=directory / "plot_to_self_suite_comparison.py"
    )
    args = parser.parse_args()
    if args.trials < 5:
        parser.error("--trials must be at least 5 for outlier filtering")
    return args


def scenario_command(args: argparse.Namespace, operation: str, send: str, recv: str,
                     output: Path) -> list[str]:
    """Build one invocation of the paired single-scenario runner."""
    min_work_bytes = (
        args.communication_min_work_bytes if operation.startswith("i") else args.min_work_bytes
    )
    command = [
        sys.executable, str(args.runner.expanduser().resolve()),
        "--base", str(args.base.expanduser()), "--base-label", args.base_label,
        "--operation", operation, "--send", send, "--recv", recv,
        "--source", str(args.source.expanduser().resolve()), "--cflags", args.cflags,
        "--output", str(output), "--data", args.data,
        "--cycles", str(args.cycles), "--trials", str(args.trials),
        "--warmups", str(args.warmups), "--min-work-bytes", str(min_work_bytes),
        "--repetitions", str(args.repetitions), "--exclude-hand-made", "--no-plot",
    ]
    for label, path in args.against:
        command.extend(("--against", label, path))
    if args.preserve_full_order:
        command.append("--preserve-full-order")
    if args.command_prefix:
        command.extend(("--command-prefix", args.command_prefix))
    for argument in args.launcher_arg:
        command.extend(("--launcher-arg", argument))
    if args.force:
        command.append("--force")
    return command


def merge_summaries(output: Path, scenario_outputs: list[tuple[str, Path]]) -> Path:
    """Combine identically shaped scenario summaries for the suite plotter."""
    merged = output / "summary.tsv"
    fieldnames: list[str] | None = None
    with merged.open("w", newline="") as destination:
        writer = None
        for scenario, directory in scenario_outputs:
            path = directory / "summary.tsv"
            with path.open(newline="") as source:
                reader = csv.DictReader(source, delimiter="\t")
                if reader.fieldnames is None:
                    raise RuntimeError(f"{path} has no header")
                if fieldnames is None:
                    fieldnames = reader.fieldnames
                    writer = csv.DictWriter(
                        destination, fieldnames=fieldnames, delimiter="\t", lineterminator="\n"
                    )
                    writer.writeheader()
                elif reader.fieldnames != fieldnames:
                    raise RuntimeError(f"{path} has incompatible columns")
                for row in reader:
                    if row["operation"] != scenario:
                        raise RuntimeError(f"{path} reports {row['operation']} instead of {scenario}")
                    writer.writerow(row)
    return merged


def main() -> int:
    """Run each scenario, merge summaries, and generate per-datatype figures."""
    args = parse_args()
    output = args.output.expanduser().resolve()
    output.mkdir(parents=True, exist_ok=True)
    commands_path = output / "commands.log"
    scenario_outputs: list[tuple[str, Path]] = []
    config = {
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "base": str(args.base),
        "base_label": args.base_label,
        "against": args.against,
        "source": str(args.source),
        "cflags": args.cflags,
        "data": args.data,
        "cycles": args.cycles,
        "trials": args.trials,
        "warmups": args.warmups,
        "min_work_bytes": args.min_work_bytes,
        "communication_min_work_bytes": args.communication_min_work_bytes,
        "repetitions": args.repetitions,
        "scenarios": [scenario for _, _, _, scenario in SCENARIOS],
    }
    (output / "run_config.json").write_text(json.dumps(config, indent=2, sort_keys=True) + "\n")
    with commands_path.open("w") as command_log:
        for index, (operation, send, recv, scenario) in enumerate(SCENARIOS, 1):
            scenario_output = output / scenario
            scenario_outputs.append((scenario, scenario_output))
            command = scenario_command(args, operation, send, recv, scenario_output)
            command_log.write(shlex.join(command) + "\n")
            command_log.flush()
            print(f"scenario [{index}/{len(SCENARIOS)}] {scenario}", flush=True)
            subprocess.run(command, check=True)

    summary = merge_summaries(output, scenario_outputs)
    if not args.no_plot:
        subprocess.run(
            [
                sys.executable, str(args.plotter.expanduser().resolve()),
                "--input", str(summary), "--output", str(output / "graphs"),
            ],
            check=True,
        )
    print(f"Results: {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, subprocess.SubprocessError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
