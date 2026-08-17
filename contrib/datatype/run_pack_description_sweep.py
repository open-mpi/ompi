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
BACKENDS = ("mpi", "current", "reference", "accelerator", "general")


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


def integer_list(value: str, option: str, minimum: int = 1) -> list[int]:
    """Parse a unique comma-delimited list of integers at or above ``minimum``."""
    result: list[int] = []
    for token in value.split(","):
        try:
            parsed = int(token.strip())
        except ValueError as error:
            raise RuntimeError(f"{option} contains an invalid value: {token}") from error
        if parsed < minimum:
            raise RuntimeError(f"{option} contains a value below {minimum}: {token}")
        if parsed not in result:
            result.append(parsed)
    if not result:
        raise RuntimeError(f"{option} cannot be empty")
    return result


def backend_list(value: str) -> list[str]:
    """Parse a unique comma-delimited list of pack_description_sweep backends."""
    result: list[str] = []
    for token in value.split(","):
        name = token.strip()
        if name not in BACKENDS:
            raise RuntimeError(
                f"unknown --backend {name!r}; expected one of {', '.join(BACKENDS)}"
            )
        if name not in result:
            result.append(name)
    if not result:
        raise RuntimeError("--backend cannot be empty")
    return result


def default_loop_items(total_items: int) -> list[int]:
    """Return every useful loop-body size, including factors that leave a DATA tail."""
    return list(range(1, total_items + 1))


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Sweep pack or unpack descriptors over copy, loop, backend, and fragment shapes."
    )
    parser.add_argument(
        "--tester", type=Path, required=True, help="pack_description_sweep executable"
    )
    parser.add_argument("--output", type=Path, required=True, help="result directory")
    parser.add_argument("--operation", choices=("pack", "unpack"), default="pack")
    parser.add_argument(
        "--backend", default="mpi",
        help="comma-delimited tester backends: mpi, current, reference, accelerator, general "
             "(default: mpi). Swept as a matrix dimension.",
    )
    parser.add_argument(
        "--fragment-bytes", default="0",
        help="comma-delimited convertor fragment sizes in bytes; 0 is one full packed buffer "
             "(default: 0). Nonzero values require a non-mpi backend and are swept with it.",
    )
    parser.add_argument(
        "--commit-description", action="store_true",
        help="measure the optimizer's committed description instead of the synthetic descriptor",
    )
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


SIGNATURE_INT_FIELDS = (
    "fragment_bytes", "element_size", "data_count", "blocklen", "block_gap", "item_gap",
    "total_items", "loop_items", "loop_iterations", "tail_items", "datatype_count",
    "packed_bytes", "cycles", "trials", "warmups",
)
SIGNATURE_STR_FIELDS = ("operation", "backend", "description")


def parse_signature(record: dict[str, str]) -> dict[str, object]:
    """Parse the tester SIGNATURE line and require every field it is specified to emit."""
    missing = [name for name in SIGNATURE_STR_FIELDS + SIGNATURE_INT_FIELDS if name not in record]
    if missing:
        raise RuntimeError(f"SIGNATURE is missing {', '.join(missing)}")
    parsed: dict[str, object] = {name: record[name] for name in SIGNATURE_STR_FIELDS}
    for name in SIGNATURE_INT_FIELDS:
        try:
            parsed[name] = int(record[name])
        except ValueError as error:
            raise RuntimeError(f"SIGNATURE {name} is not an integer: {record[name]}") from error
    return parsed


def check_signature(signature: dict[str, object], expected: dict[str, object], command: str) -> None:
    """Reject a SIGNATURE that disagrees with the request for fields the tester must echo."""
    differing = [
        f"{name} requested={expected[name]!r} signature={signature[name]!r}"
        for name in expected if signature[name] != expected[name]
    ]
    if differing:
        raise RuntimeError(
            "tester SIGNATURE disagrees with the request ("
            + "; ".join(differing)
            + f"): {command}"
        )


def loop_shape_decomposed(signature: dict[str, object]) -> bool:
    """Return whether loop_items/iterations/tail_items are real counts, not -1 sentinels."""
    return (
        int(signature["loop_items"]) >= 0
        and int(signature["loop_iterations"]) >= 0
        and int(signature["tail_items"]) >= 0
    )


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
        f"backend={args.backend}",
        f"fragment_bytes={args.fragment_bytes}",
        f"commit_description={int(args.commit_description)}",
        f"element_size={args.element_size}",
        f"matrix_size={matrix_size}",
    ]
    (args.output / "manifest.txt").write_text("\n".join(lines) + "\n")


def summarize(rows: list[dict[str, object]], equivalent_elements: int) -> list[dict[str, object]]:
    """Aggregate repeated tester results and compare loop groupings with the one-item baseline."""
    grouped: dict[tuple[object, ...], list[dict[str, object]]] = {}
    for row in rows:
        key = (
            str(row["backend"]), int(row["fragment_bytes"]), str(row["description"]),
            int(row["block_gap"]), int(row["item_gap"]), int(row["data_count"]),
            int(row["blocklen"]), int(row["requested_loop_items"]),
        )
        grouped.setdefault(key, []).append(row)

    summaries: list[dict[str, object]] = []
    baseline: dict[tuple[object, ...], float] = {}
    for key, group in grouped.items():
        means = [float(row["mean_seconds"]) for row in group]
        if any((not math.isfinite(value)) or (value <= 0.0) for value in means):
            raise ValueError(
                "non-positive or non-finite mean_seconds for config "
                f"backend={key[0]} fragment_bytes={key[1]} description={key[2]} "
                f"block_gap={key[3]} item_gap={key[4]} data_count={key[5]} "
                f"blocklen={key[6]} requested_loop_items={key[7]}: {means}"
            )
        median = statistics.median(means)
        decomposed = all(int(row["loop_decomposed"]) for row in group)
        signature_items = int(group[0]["loop_items"])
        # The loop-items=1 baseline is only meaningful when that request still
        # ran as a one-item loop.  A -1 sentinel or an optimizer rewrite would
        # make speedup_vs_loop_items_1_pct compare unrelated shapes.
        if decomposed and 1 == key[7] and 1 == signature_items:
            baseline[key[:7]] = median
        mean = statistics.fmean(means)
        repeat_stddev = statistics.stdev(means) if 1 < len(means) else 0.0
        summaries.append(
            {
                "backend": key[0],
                "fragment_bytes": key[1],
                "description": key[2],
                "block_gap": key[3],
                "item_gap": key[4],
                "data_count": key[5],
                "blocklen": key[6],
                "requested_loop_items": key[7],
                "loop_items": signature_items,
                "loop_iterations": int(group[0]["loop_iterations"]),
                "tail_items": int(group[0]["tail_items"]),
                "loop_decomposed": int(decomposed),
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
        family = (
            row["backend"], int(row["fragment_bytes"]), row["description"],
            int(row["block_gap"]), int(row["item_gap"]), int(row["data_count"]),
            int(row["blocklen"]),
        )
        reference = baseline.get(family)
        # Only compare against loop_items=1 when this row's committed/synthetic
        # shape is still the requested loop factor.
        if (
            reference is not None
            and int(row["loop_decomposed"])
            and int(row["loop_items"]) == int(row["requested_loop_items"])
        ):
            row["speedup_vs_loop_items_1_pct"] = (
                100.0 * (reference / float(row["median_seconds"]) - 1.0)
            )
        else:
            row["speedup_vs_loop_items_1_pct"] = ""
    if equivalent_elements:
        has_fused_cell = any(
            (1 == int(row["data_count"])) and (equivalent_elements == int(row["blocklen"]))
            for row in summaries
        )
        if not has_fused_cell:
            raise RuntimeError("--equivalent-elements requires the fused count=1 shape")
        fused = {
            (
                row["backend"], int(row["fragment_bytes"]), row["description"],
                int(row["block_gap"]), int(row["item_gap"]), int(row["requested_loop_items"]),
            ):
            float(row["median_seconds"])
            for row in summaries
            if (1 == int(row["data_count"]))
            and (equivalent_elements == int(row["blocklen"]))
            and int(row["loop_decomposed"])
        }
        for row in summaries:
            fused_key = (
                row["backend"], int(row["fragment_bytes"]), row["description"],
                int(row["block_gap"]), int(row["item_gap"]), int(row["requested_loop_items"]),
            )
            reference = fused.get(fused_key)
            if reference is not None and int(row["loop_decomposed"]):
                row["speedup_vs_fused_copy_pct"] = (
                    100.0 * (reference / float(row["median_seconds"]) - 1.0)
                )
            else:
                row["speedup_vs_fused_copy_pct"] = ""
    return sorted(
        summaries,
        key=lambda row: (
            row["backend"], row["fragment_bytes"], row["description"],
            row["block_gap"], row["item_gap"], row["data_count"], row["blocklen"],
            row["requested_loop_items"],
        ),
    )


def main() -> int:
    args = parse_args()
    tester = args.tester.expanduser().resolve()
    if not tester.is_file():
        raise RuntimeError(f"tester does not exist: {tester}")

    data_counts = integer_list(args.data_counts, "--data-counts")
    blocklens = integer_list(args.blocklens, "--blocklens")
    backends = backend_list(args.backend)
    fragments = integer_list(args.fragment_bytes, "--fragment-bytes", minimum=0)
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
    if args.commit_description and any(0 != args.total_items % value for value in loop_items):
        raise RuntimeError("--loop-items must divide --total-items with --commit-description")

    copy_shapes = list(itertools.product(data_counts, blocklens))
    if args.equivalent_elements:
        copy_shapes = [
            shape for shape in copy_shapes if args.equivalent_elements == shape[0] * shape[1]
        ]
        if not copy_shapes:
            raise RuntimeError("--equivalent-elements has no matching count/blocklen pair")
        if (1, args.equivalent_elements) not in copy_shapes:
            raise RuntimeError("--equivalent-elements requires data-count 1 and its blocklen")
    matrix = []
    skipped_pairs: list[tuple[str, int]] = []
    for backend, fragment, (data_count, blocklen), items in itertools.product(
        backends, fragments, copy_shapes, loop_items
    ):
        if "mpi" == backend and 0 != fragment:
            if (backend, fragment) not in skipped_pairs:
                skipped_pairs.append((backend, fragment))
            continue
        if "pack" == args.operation and 0 != fragment and fragment < args.element_size:
            if (backend, fragment) not in skipped_pairs:
                skipped_pairs.append((backend, fragment))
            continue
        matrix.append((backend, fragment, data_count, blocklen, items))
    if skipped_pairs:
        print(
            "warning: skipped backend/fragment combinations the tester rejects: "
            + ", ".join(f"{backend}@{fragment}" for backend, fragment in skipped_pairs),
            file=sys.stderr,
        )
    if not matrix:
        raise RuntimeError(
            "no valid backend/fragment-bytes combinations; --fragment-bytes requires "
            "current, reference, accelerator, or general, and pack fragments must be "
            "at least --element-size"
        )
    args.output.mkdir(parents=True, exist_ok=True)
    raw_path = args.output / "raw.tsv"
    summary_path = args.output / "summary.tsv"
    if not args.force and (raw_path.exists() or summary_path.exists()):
        raise RuntimeError(f"result tables already exist under {args.output}; use --force to overwrite")

    config = {
        "tester": str(tester),
        "tester_sha256": sha256(tester),
        "operation": args.operation,
        "backend": backends,
        "fragment_bytes": fragments,
        "commit_description": args.commit_description,
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
    undecomposed = 0
    with (args.output / "commands.tsv").open("w") as command_log, (
        args.output / "program-output.log"
    ).open("w") as output_log:
        command_log.write(
            "backend\tfragment_bytes\tblock_gap\titem_gap\tdata_count\tblocklen\t"
            "loop_items\trepetition\tcommand\n"
        )
        for repetition in range(args.repetitions):
            round_matrix = matrix if 0 == repetition % 2 else list(reversed(matrix))
            for backend, fragment, data_count, blocklen, items in round_matrix:
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
                    f"--backend={backend}",
                    f"--fragment-bytes={fragment}",
                ]
                if args.commit_description:
                    command.append("--commit-description")
                print(
                    f"[{run_index}/{total_runs}] backend={backend} fragment={fragment} "
                    f"repetition={repetition} count={data_count} blocklen={blocklen} "
                    f"loop_items={items}",
                    flush=True,
                )
                command_log.write(
                    f"{backend}\t{fragment}\t{args.block_gap}\t{args.item_gap}\t"
                    f"{data_count}\t{blocklen}\t{items}\t{repetition}\t{shlex.join(command)}\n"
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
                parsed = parse_signature(signature)
                expected: dict[str, object] = {
                    "operation": args.operation,
                    "backend": backend,
                    "fragment_bytes": fragment,
                    "description": "commit" if args.commit_description else "synthetic",
                    "element_size": args.element_size,
                    "data_count": data_count,
                    "blocklen": blocklen,
                    "block_gap": args.block_gap,
                    "item_gap": args.item_gap,
                    "total_items": args.total_items,
                    "datatype_count": args.datatype_count,
                    "trials": args.trials,
                    "warmups": args.warmups,
                }
                if not args.commit_description:
                    expected["loop_items"] = items
                    expected["loop_iterations"] = args.total_items // items
                    expected["tail_items"] = args.total_items % items
                check_signature(parsed, expected, shlex.join(command))
                decomposed = loop_shape_decomposed(parsed)
                if not decomposed:
                    if not args.commit_description:
                        raise RuntimeError(
                            "synthetic SIGNATURE reported an undecomposed loop shape: "
                            + shlex.join(command)
                        )
                    undecomposed += 1
                record = results[0]
                rows.append(
                    {
                        "operation": parsed["operation"],
                        "backend": parsed["backend"],
                        "fragment_bytes": parsed["fragment_bytes"],
                        "description": parsed["description"],
                        "element_size": parsed["element_size"],
                        "block_gap": parsed["block_gap"],
                        "item_gap": parsed["item_gap"],
                        "data_count": parsed["data_count"],
                        "blocklen": parsed["blocklen"],
                        "total_items": parsed["total_items"],
                        "requested_loop_items": items,
                        "loop_items": parsed["loop_items"],
                        "loop_iterations": parsed["loop_iterations"],
                        "tail_items": parsed["tail_items"],
                        "loop_decomposed": int(decomposed),
                        "datatype_count": parsed["datatype_count"],
                        "packed_bytes": parsed["packed_bytes"],
                        "cycles": parsed["cycles"],
                        "trials": parsed["trials"],
                        "warmups": parsed["warmups"],
                        "repetition": repetition,
                        "mean_seconds": float(record["mean_seconds"]),
                        "trial_stddev_seconds": float(record["stddev_seconds"]),
                        "trial_stddev_pct": float(record["stddev_pct"]),
                        "min_seconds": float(record["min_seconds"]),
                        "max_seconds": float(record["max_seconds"]),
                        "bandwidth_mib_s": float(record["bandwidth_mib_s"]),
                    }
                )

    if undecomposed:
        print(
            f"warning: {undecomposed} committed run(s) reported loop_items/loop_iterations/"
            "tail_items as -1 (descriptor not decomposable into whole items); those rows "
            "set loop_decomposed=0 and omit speedup_vs_loop_items_1",
            file=sys.stderr,
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
