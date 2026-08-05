#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

"""Sweep exact internal pack or unpack descriptions and retain timing distributions."""

from __future__ import annotations

import argparse
import csv
import itertools
import json
import math
import os
import platform
import shlex
import statistics
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from datatype_bench_common import cpu_description, sha256  # noqa: E402


DEFAULT_DATA_COUNTS = "1,2,4,8,16"
DEFAULT_BLOCKLENS = "1,2,3,4,5,6,7,8,9,16,32,64,128"


def positive_int(value: str) -> int:
    """Parse one positive command-line integer."""
    parsed = int(value)
    if parsed < 1:
        raise argparse.ArgumentTypeError("value must be positive")
    return parsed


def nonnegative_int(value: str) -> int:
    """Parse one nonnegative command-line integer."""
    parsed = int(value)
    if parsed < 0:
        raise argparse.ArgumentTypeError("value cannot be negative")
    return parsed


def integer_list(value: str, option: str) -> list[int]:
    """Parse a unique comma-delimited list of positive integers."""
    result: list[int] = []
    for token in value.split(","):
        try:
            parsed = positive_int(token.strip())
        except (ValueError, argparse.ArgumentTypeError) as error:
            raise RuntimeError(f"{option} contains an invalid value: {token}") from error
        if parsed not in result:
            result.append(parsed)
    if not result:
        raise RuntimeError(f"{option} cannot be empty")
    return result


def default_loop_items(total_items: int) -> list[int]:
    """Return every useful loop-body size, including factors that leave a DATA tail."""
    return list(range(1, total_items + 1))


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Sweep exact synthetic pack or unpack descriptors over copy and loop shapes."
    )
    parser.add_argument(
        "--tester", type=Path, required=True, help="pack_description_sweep executable"
    )
    parser.add_argument("--output", type=Path, required=True, help="result directory")
    parser.add_argument("--operation", choices=("pack", "unpack"), default="pack")
    parser.add_argument("--element-size", choices=(4, 8), type=int, default=4)
    parser.add_argument("--data-counts", default=DEFAULT_DATA_COUNTS)
    parser.add_argument("--blocklens", default=DEFAULT_BLOCKLENS)
    parser.add_argument(
        "--loop-items",
        default="auto",
        help="comma-delimited loop-body item counts; auto uses every value through total-items",
    )
    parser.add_argument("--total-items", type=positive_int, default=16)
    parser.add_argument("--block-gap", type=nonnegative_int, default=1)
    parser.add_argument("--item-gap", type=nonnegative_int, default=1)
    parser.add_argument(
        "--equivalent-elements", type=nonnegative_int, default=0,
        help="retain only count/blocklen pairs whose product equals this value",
    )
    parser.add_argument("--datatype-count", type=positive_int, default=1)
    parser.add_argument("--cycles", type=positive_int, default=100)
    parser.add_argument("--trials", type=positive_int, default=20)
    parser.add_argument("--warmups", type=nonnegative_int, default=2)
    parser.add_argument("--repetitions", type=positive_int, default=5)
    parser.add_argument("--min-work-bytes", type=nonnegative_int, default=1024 * 1024)
    parser.add_argument(
        "--command-prefix",
        default="",
        help="command placed before the tester, for example 'taskset -c 3'",
    )
    parser.add_argument("--force", action="store_true", help="overwrite result tables")
    args = parser.parse_args()

    if args.trials < 2:
        parser.error("--trials must be at least 2 to compute standard deviation")
    if args.equivalent_elements and (0 != args.block_gap or 0 == args.item_gap):
        parser.error(
            "--equivalent-elements requires --block-gap=0 and a nonzero --item-gap"
        )
    return args


def parse_record(line: str, prefix: str) -> dict[str, str] | None:
    """Parse one space-delimited tester record containing key=value fields."""
    if not line.startswith(prefix + " "):
        return None
    record: dict[str, str] = {}
    for field in shlex.split(line)[1:]:
        key, separator, value = field.partition("=")
        if not separator:
            raise RuntimeError(f"malformed {prefix} field: {field}")
        record[key] = value
    return record


def write_manifest(args: argparse.Namespace, tester: Path, matrix_size: int) -> None:
    """Record the machine, binary, and matrix controls needed to reproduce the sweep."""
    lines = [
        f"timestamp_utc={datetime.now(timezone.utc).isoformat()}",
        f"hostname={platform.node()}",
        f"platform={platform.platform()}",
        f"machine={platform.machine()}",
        f"processor={cpu_description()}",
        f"python={platform.python_version()}",
        f"tester={tester}",
        f"tester_sha256={sha256(tester)}",
        f"operation={args.operation}",
        f"element_size={args.element_size}",
        f"matrix_size={matrix_size}",
    ]
    (args.output / "manifest.txt").write_text("\n".join(lines) + "\n")


def summarize(rows: list[dict[str, object]], equivalent_elements: int) -> list[dict[str, object]]:
    """Aggregate repeated tester results and compare loop groupings with the one-item baseline."""
    grouped: dict[tuple[int, int, int, int, int], list[dict[str, object]]] = {}
    for row in rows:
        key = (
            int(row["block_gap"]), int(row["item_gap"]), int(row["data_count"]),
            int(row["blocklen"]), int(row["loop_items"]),
        )
        grouped.setdefault(key, []).append(row)

    summaries: list[dict[str, object]] = []
    baseline: dict[tuple[int, int, int, int], float] = {}
    for key, group in grouped.items():
        means = [float(row["mean_seconds"]) for row in group]
        if any((not math.isfinite(value)) or (value <= 0.0) for value in means):
            raise ValueError(
                "non-positive or non-finite mean_seconds for config "
                f"block_gap={key[0]} item_gap={key[1]} data_count={key[2]} "
                f"blocklen={key[3]} loop_items={key[4]}: {means}"
            )
        median = statistics.median(means)
        if 1 == key[4]:
            baseline[key[:4]] = median
        mean = statistics.fmean(means)
        repeat_stddev = statistics.stdev(means) if 1 < len(means) else 0.0
        summaries.append(
            {
                "block_gap": key[0],
                "item_gap": key[1],
                "data_count": key[2],
                "blocklen": key[3],
                "loop_items": key[4],
                "loop_iterations": int(group[0]["loop_iterations"]),
                "tail_items": int(group[0]["tail_items"]),
                "packed_bytes": int(group[0]["packed_bytes"]),
                "repetitions": len(group),
                "mean_seconds": mean,
                "median_seconds": median,
                "repeat_stddev_seconds": repeat_stddev,
                "repeat_stddev_pct": 100.0 * repeat_stddev / mean,
                "mean_trial_stddev_pct": statistics.fmean(
                    float(row["trial_stddev_pct"]) for row in group
                ),
                "min_mean_seconds": min(means),
                "max_mean_seconds": max(means),
                "median_bandwidth_mib_s": int(group[0]["packed_bytes"])
                / median
                / (1024.0 * 1024.0),
            }
        )

    for row in summaries:
        reference = baseline[
            (
                int(row["block_gap"]), int(row["item_gap"]), int(row["data_count"]),
                int(row["blocklen"]),
            )
        ]
        row["speedup_vs_loop_items_1_pct"] = 100.0 * (reference / float(row["median_seconds"]) - 1.0)
    if equivalent_elements:
        fused = {
            (int(row["block_gap"]), int(row["item_gap"]), int(row["loop_items"])):
            float(row["median_seconds"])
            for row in summaries
            if (1 == int(row["data_count"]))
            and (equivalent_elements == int(row["blocklen"]))
        }
        if not fused:
            raise RuntimeError("--equivalent-elements requires the fused count=1 shape")
        for row in summaries:
            reference = fused[
                (int(row["block_gap"]), int(row["item_gap"]), int(row["loop_items"]))
            ]
            row["speedup_vs_fused_copy_pct"] = (
                100.0 * (reference / float(row["median_seconds"]) - 1.0)
            )
    return sorted(
        summaries,
        key=lambda row: (
            row["block_gap"], row["item_gap"], row["data_count"], row["blocklen"],
            row["loop_items"],
        ),
    )


def main() -> int:
    args = parse_args()
    tester = args.tester.expanduser().resolve()
    if not tester.is_file():
        raise RuntimeError(f"tester does not exist: {tester}")

    data_counts = integer_list(args.data_counts, "--data-counts")
    blocklens = integer_list(args.blocklens, "--blocklens")
    loop_items = (
        default_loop_items(args.total_items)
        if "auto" == args.loop_items
        else integer_list(args.loop_items, "--loop-items")
    )
    invalid_loop_items = [value for value in loop_items if args.total_items < value]
    if invalid_loop_items:
        raise RuntimeError("every --loop-items value must be no larger than --total-items")
    if 1 not in loop_items:
        raise RuntimeError("--loop-items must include 1 as the speedup baseline")

    copy_shapes = list(itertools.product(data_counts, blocklens))
    if args.equivalent_elements:
        copy_shapes = [
            shape for shape in copy_shapes if args.equivalent_elements == shape[0] * shape[1]
        ]
        if not copy_shapes:
            raise RuntimeError("--equivalent-elements has no matching count/blocklen pair")
        if (1, args.equivalent_elements) not in copy_shapes:
            raise RuntimeError("--equivalent-elements requires data-count 1 and its blocklen")
    matrix = [(data_count, blocklen, items) for data_count, blocklen in copy_shapes for items in loop_items]
    args.output.mkdir(parents=True, exist_ok=True)
    raw_path = args.output / "raw.tsv"
    summary_path = args.output / "summary.tsv"
    if not args.force and (raw_path.exists() or summary_path.exists()):
        raise RuntimeError(f"result tables already exist under {args.output}; use --force to overwrite")

    config = {
        "tester": str(tester),
        "tester_sha256": sha256(tester),
        "operation": args.operation,
        "element_size": args.element_size,
        "data_counts": data_counts,
        "blocklens": blocklens,
        "loop_items": loop_items,
        "total_items": args.total_items,
        "block_gap": args.block_gap,
        "item_gap": args.item_gap,
        "equivalent_elements": args.equivalent_elements,
        "datatype_count": args.datatype_count,
        "cycles": args.cycles,
        "trials": args.trials,
        "warmups": args.warmups,
        "repetitions": args.repetitions,
        "min_work_bytes": args.min_work_bytes,
        "command_prefix": args.command_prefix,
    }
    (args.output / "run_config.json").write_text(json.dumps(config, indent=2, sort_keys=True) + "\n")
    write_manifest(args, tester, len(matrix))

    command_prefix = shlex.split(args.command_prefix)
    environment = os.environ.copy()
    environment["OMPI_MCA_btl"] = "self"
    rows: list[dict[str, object]] = []
    total_runs = len(matrix) * args.repetitions
    run_index = 0
    with (args.output / "commands.tsv").open("w") as command_log, (
        args.output / "program-output.log"
    ).open("w") as output_log:
        command_log.write("block_gap\titem_gap\tdata_count\tblocklen\tloop_items\trepetition\tcommand\n")
        for repetition in range(args.repetitions):
            round_matrix = matrix if 0 == repetition % 2 else list(reversed(matrix))
            for data_count, blocklen, items in round_matrix:
                run_index += 1
                command = command_prefix + [
                    str(tester),
                    f"--data-count={data_count}",
                    f"--blocklen={blocklen}",
                    f"--element-size={args.element_size}",
                    f"--block-gap={args.block_gap}",
                    f"--item-gap={args.item_gap}",
                    f"--total-items={args.total_items}",
                    f"--loop-items={items}",
                    f"--datatype-count={args.datatype_count}",
                    f"--cycles={args.cycles}",
                    f"--trials={args.trials}",
                    f"--warmups={args.warmups}",
                    "--repetitions=1",
                    f"--min-work-bytes={args.min_work_bytes}",
                    f"--operation={args.operation}",
                ]
                print(
                    f"[{run_index}/{total_runs}] repetition={repetition} count={data_count} "
                    f"blocklen={blocklen} loop_items={items}",
                    flush=True,
                )
                command_log.write(
                    f"{args.block_gap}\t{args.item_gap}\t{data_count}\t{blocklen}\t{items}\t"
                    f"{repetition}\t{shlex.join(command)}\n"
                )
                command_log.flush()
                result = subprocess.run(command, env=environment, text=True, capture_output=True)
                output_log.write(f"$ {shlex.join(command)}\n{result.stdout}{result.stderr}\n")
                output_log.flush()
                if 0 != result.returncode:
                    raise RuntimeError(f"tester failed ({result.returncode}): {shlex.join(command)}")

                signature = next(
                    (
                        record
                        for line in result.stdout.splitlines()
                        if (record := parse_record(line, "SIGNATURE"))
                    ),
                    None,
                )
                results = [
                    record
                    for line in result.stdout.splitlines()
                    if (record := parse_record(line, "RESULT"))
                ]
                if signature is None or 1 != len(results):
                    raise RuntimeError(f"unexpected tester output: {shlex.join(command)}")
                if args.operation != signature.get("operation"):
                    raise RuntimeError(f"tester reported the wrong operation: {shlex.join(command)}")
                record = results[0]
                rows.append(
                    {
                        "operation": args.operation,
                        "element_size": args.element_size,
                        "block_gap": int(signature["block_gap"]),
                        "item_gap": int(signature["item_gap"]),
                        "data_count": data_count,
                        "blocklen": blocklen,
                        "total_items": args.total_items,
                        "loop_items": items,
                        "loop_iterations": int(signature["loop_iterations"]),
                        "tail_items": int(signature["tail_items"]),
                        "datatype_count": args.datatype_count,
                        "packed_bytes": int(signature["packed_bytes"]),
                        "cycles": int(signature["cycles"]),
                        "trials": args.trials,
                        "warmups": args.warmups,
                        "repetition": repetition,
                        "mean_seconds": float(record["mean_seconds"]),
                        "trial_stddev_seconds": float(record["stddev_seconds"]),
                        "trial_stddev_pct": float(record["stddev_pct"]),
                        "min_seconds": float(record["min_seconds"]),
                        "max_seconds": float(record["max_seconds"]),
                        "bandwidth_mib_s": float(record["bandwidth_mib_s"]),
                    }
                )

    raw_fields = list(rows[0])
    with raw_path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=raw_fields, delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)

    summary_rows = summarize(rows, args.equivalent_elements)
    with summary_path.open("w", newline="") as stream:
        writer = csv.DictWriter(
            stream, fieldnames=list(summary_rows[0]), delimiter="\t", lineterminator="\n"
        )
        writer.writeheader()
        writer.writerows(summary_rows)
    print(f"Results: {args.output}")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError, subprocess.SubprocessError, ValueError) as error:
        # ValueError covers summarize()'s explicit raise plus statistics/float()
        # conversion failures on malformed tester output.
        print(f"error: {error}", file=sys.stderr)
        sys.exit(1)
