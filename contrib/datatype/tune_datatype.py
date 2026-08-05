#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

"""Measure architecture-sensitive Open MPI datatype pack and unpack policies."""

from __future__ import annotations

import argparse
import csv
import itertools
import json
import math
import os
import platform
import re
import shlex
import statistics
import subprocess
import sys
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

sys.path.insert(0, str(Path(__file__).resolve().parent))
from datatype_bench_common import cpu_description, sha256  # noqa: E402


SOURCE_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_THRESHOLDS = "0,64,128,250,512,1024"
NOISE_PCT = 3.0


@dataclass(frozen=True)
class SweepCase:
    """One exact synthetic descriptor and convertor execution policy."""

    stage: str
    operation: str
    backend: str
    element_size: int
    data_count: int
    blocklen: int
    block_gap: int
    item_gap: int
    total_items: int
    loop_items: int
    datatype_count: int
    fragment_bytes: int

    @property
    def packed_bytes(self) -> int:
        return (
            self.element_size
            * self.data_count
            * self.blocklen
            * self.total_items
            * self.datatype_count
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


def integer_list(value: str) -> list[int]:
    """Parse a comma-delimited list while preserving order."""
    result: list[int] = []
    for token in value.split(","):
        parsed = nonnegative_int(token.strip())
        if parsed not in result:
            result.append(parsed)
    if not result:
        raise argparse.ArgumentTypeError("list cannot be empty")
    return result


def check_list(value: str) -> list[str]:
    """Parse and validate the requested tuning stages."""
    valid = {"corpus", "loops", "movers"}
    result = [token.strip() for token in value.split(",") if token.strip()]
    if "all" in result:
        return ["corpus", "loops", "movers"]
    unknown = sorted(set(result) - valid)
    if unknown or not result:
        raise argparse.ArgumentTypeError(f"valid checks are all,{','.join(sorted(valid))}")
    return list(dict.fromkeys(result))


def parse_args() -> argparse.Namespace:
    """Parse paths, launcher controls, and statistical effort."""
    in_tree_build = SOURCE_ROOT / "build" / "release"
    sibling_build = SOURCE_ROOT.parent / "build" / "release"
    default_build = in_tree_build if in_tree_build.is_dir() else sibling_build
    parser = argparse.ArgumentParser(
        description="Tune Open MPI datatype pack/unpack policies with controlled mpirun jobs."
    )
    parser.add_argument("--build-dir", type=Path, default=default_build)
    parser.add_argument("--mpirun", type=Path, help="launcher; defaults to BUILD/bin/mpirun or PATH")
    parser.add_argument("--to-self", type=Path, help="to_self executable; defaults to BUILD/test/datatype")
    parser.add_argument(
        "--description-sweep", type=Path,
        help="pack_description_sweep executable; defaults to BUILD/test/datatype",
    )
    parser.add_argument("--mpirun-arg", action="append", default=[], help="repeat for launcher options")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--profile", choices=("quick", "full"), default="quick")
    parser.add_argument("--check", type=check_list, default=check_list("all"))
    parser.add_argument("--cycles", type=positive_int, default=100)
    parser.add_argument("--trials", type=positive_int, default=20)
    parser.add_argument("--warmups", type=nonnegative_int, default=2)
    parser.add_argument("--repetitions", type=positive_int, default=5)
    parser.add_argument("--min-work-bytes", type=nonnegative_int, default=1024 * 1024)
    parser.add_argument("--noise-pct", type=float, default=NOISE_PCT)
    parser.add_argument("--consolidation-thresholds", type=integer_list, default=integer_list(DEFAULT_THRESHOLDS))
    parser.add_argument("--corpus-checks", default="pack,unpack")
    parser.add_argument("--corpus-data", default="all", help="to_self --data selection")
    parser.add_argument("--force", action="store_true", help="overwrite this app's result files")
    args = parser.parse_args()
    if args.trials < 2:
        parser.error("--trials must be at least 2")
    if not math.isfinite(args.noise_pct) or args.noise_pct < 0.0:
        parser.error("--noise-pct must be finite and nonnegative")
    return args


def cache_description() -> dict[str, int | str]:
    """Record cache information used to interpret fragment crossover measurements."""
    result: dict[str, int | str] = {"line_bytes": "unknown", "l1_data_bytes": "unknown"}
    if sys.platform == "darwin":
        keys = {"hw.cachelinesize": "line_bytes", "hw.l1dcachesize": "l1_data_bytes"}
        for key, output_name in keys.items():
            query = subprocess.run(["sysctl", "-n", key], text=True, capture_output=True)
            if 0 == query.returncode and query.stdout.strip().isdigit():
                result[output_name] = int(query.stdout.strip())
        return result
    sysfs = Path("/sys/devices/system/cpu/cpu0/cache")
    for index in sysfs.glob("index*"):
        try:
            level = (index / "level").read_text().strip()
            cache_type = (index / "type").read_text().strip()
            line_size = int((index / "coherency_line_size").read_text().strip())
            size_text = (index / "size").read_text().strip().upper()
        except (OSError, ValueError):
            continue
        if "1" == level and cache_type in ("Data", "Unified"):
            result["line_bytes"] = line_size
            # sysfs reports sizes like "32K"/"1M"; only strip a recognized unit
            # suffix, and accept a bare byte count. An unexpected format is left
            # unparsed rather than silently truncated to the wrong magnitude.
            units = {"K": 1024, "M": 1024 * 1024, "G": 1024 * 1024 * 1024}
            if size_text[-1:] in units:
                magnitude, multiplier = size_text[:-1], units[size_text[-1:]]
            else:
                magnitude, multiplier = size_text, 1
            if magnitude.isdigit():
                result["l1_data_bytes"] = int(magnitude) * multiplier
            break
    return result


def resolve_executable(candidate: Path | None, fallback: Path, name: str, option: str) -> Path:
    """Resolve one required executable from an option, build tree, or PATH."""
    if candidate is not None:
        path = candidate.expanduser().resolve()
    elif fallback.is_file():
        path = fallback.resolve()
    else:
        found = shutil_which(name)
        if found is None:
            raise RuntimeError(f"cannot find {name}; provide {option}")
        path = Path(found).resolve()
    if not path.is_file() or not os.access(path, os.X_OK):
        raise RuntimeError(f"not an executable: {path}")
    return path


def shutil_which(name: str) -> str | None:
    """Small PATH lookup kept local so the manifest records the resolved binary."""
    for directory in os.environ.get("PATH", "").split(os.pathsep):
        candidate = Path(directory) / name
        if candidate.is_file() and os.access(candidate, os.X_OK):
            return str(candidate)
    return None


def configured_mpirun(build_dir: Path) -> Path:
    """Find mpirun in the build tree or in the prefix recorded by config.status."""
    in_build = build_dir / "bin" / "mpirun"
    if in_build.is_file():
        return in_build
    config_status = build_dir / "config.status"
    if config_status.is_file() and os.access(config_status, os.X_OK):
        result = subprocess.run([str(config_status), "--config"], text=True, capture_output=True)
        if 0 == result.returncode:
            for option in shlex.split(result.stdout):
                if option.startswith("--prefix="):
                    installed = Path(option.split("=", 1)[1]) / "bin" / "mpirun"
                    if installed.is_file():
                        return installed
    return in_build


def parse_record(line: str, prefix: str) -> dict[str, str] | None:
    """Parse one benchmark record containing shell-safe key=value fields."""
    if not line.startswith(prefix + " "):
        return None
    record: dict[str, str] = {}
    for field in shlex.split(line)[1:]:
        key, separator, value = field.partition("=")
        if not separator:
            raise RuntimeError(f"malformed {prefix} field: {field}")
        record[key] = value
    return record


def largest_safe_cutoff(rows: list[dict[str, object]], key: str, minimum: int,
                        noise_pct: float) -> int:
    """Return the largest ``key`` value that is safe to use as a MAX_* cutoff.

    A MAX_* threshold forces the vectorized path on *every* shape whose ``key``
    is at or below the cutoff, so a value is only safe if no smaller measured
    shape regressed.  Walk the measured values ascending, stop at the first
    regressor (aggregate slowdown beyond the noise band), and return the largest
    value that is itself consistently beneficial before that point.  This avoids
    recommending, for example, 64 when 16 regresses in between.
    """
    recommended = 0
    for row in sorted(rows, key=lambda entry: int(entry[key])):
        value = int(row[key])
        if value <= minimum:
            continue
        geomean = float(row["geomean_speedup_pct"])
        if geomean < -noise_pct:
            break
        if geomean > noise_pct and int(row["regressions"]) <= max(1, int(row["samples"]) // 10):
            recommended = value
    return recommended


def geometric_mean(values: Iterable[float]) -> float:
    """Compute a stable geometric mean for positive timing ratios."""
    materialized = list(values)
    if not materialized or any(value <= 0.0 for value in materialized):
        raise RuntimeError("geometric mean requires positive values")
    return math.exp(statistics.fmean(math.log(value) for value in materialized))


def make_cases(profile: str, stages: list[str]) -> list[SweepCase]:
    """Build controlled matrices for loop geometry and current/reference mover selection."""
    cases: list[SweepCase] = []
    if "loops" in stages:
        data_counts = [1, 4] if "quick" == profile else [1, 2, 4, 8]
        blocklens = [1, 4, 8, 16, 64] if "quick" == profile else list(range(1, 10)) + [16, 32, 64, 128]
        loop_items = [1, 2, 4, 8] if "quick" == profile else [1, 2, 3, 4, 5, 6, 8, 16]
        shapes = itertools.product(("pack", "unpack"), (4, 8), data_counts, blocklens)
        for shape_index, (operation, element_size, data_count, blocklen) in enumerate(shapes):
            # Alternate factor order so later factors do not systematically inherit machine drift.
            ordered_items = loop_items if 0 == shape_index % 2 else list(reversed(loop_items))
            for items in ordered_items:
                cases.append(
                    SweepCase("loops", operation, "current", element_size, data_count, blocklen, 1,
                              1, 16, items, 8, 0)
                )

    if "movers" in stages:
        data_counts = [16] if "quick" == profile else [2, 16]
        blocklens = [1, 4, 8, 9, 16, 64, 128] if "quick" == profile else list(range(1, 10)) + [16, 32, 64, 128]
        block_gaps = [1, 32] if "quick" == profile else [1, 12, 24, 32, 64]
        fragments = [0, 4096, 65536, 1048576] if "quick" == profile else [0, 1024, 4096, 16384,
                                                                                  65536, 131072, 1048576]
        shapes = itertools.product(("pack", "unpack"), (4, 8), data_counts, blocklens,
                                   block_gaps, fragments)
        pair_index = 0
        for values in shapes:
            operation, element_size, data_count, blocklen, block_gap, fragment = values
            probe = SweepCase("movers", operation, "current", element_size, data_count, blocklen,
                              block_gap, 4, 16, 16, 16, fragment)
            if 0 != fragment and fragment >= probe.packed_bytes:
                continue
            # Alternate paired process order to avoid assigning all temporal drift to one backend.
            backends = ("current", "reference") if 0 == pair_index % 2 else ("reference", "current")
            pair_index += 1
            for backend in backends:
                cases.append(
                    SweepCase("movers", operation, backend, element_size, data_count, blocklen,
                              block_gap, 4, 16, 16, 16, fragment)
                )
    return cases


def launch(command: list[str], environment: dict[str, str], command_log, output_log) -> str:
    """Run one recorded command and preserve stdout/stderr before interpreting it."""
    rendered = shlex.join(command)
    command_log.write(rendered + "\n")
    command_log.flush()
    result = subprocess.run(command, env=environment, text=True, capture_output=True)
    output_log.write(f"$ {rendered}\n{result.stdout}{result.stderr}\n")
    output_log.flush()
    if 0 != result.returncode:
        raise RuntimeError(f"command failed ({result.returncode}): {rendered}")
    return result.stdout


def run_controlled_cases(args: argparse.Namespace, launcher: list[str], tester: Path,
                         environment: dict[str, str], command_log, output_log) -> list[dict[str, object]]:
    """Run exact descriptors and retain every repeated timing result."""
    cases = make_cases(args.profile, args.check)
    rows: list[dict[str, object]] = []
    for index, case in enumerate(cases, 1):
        print(f"controlled [{index}/{len(cases)}] {case.stage} {case.operation} {case.backend} "
              f"size={case.element_size} count={case.data_count} blocklen={case.blocklen} "
              f"loop={case.loop_items} fragment={case.fragment_bytes}", flush=True)
        command = launcher + [
            str(tester),
            f"--operation={case.operation}",
            f"--backend={case.backend}",
            f"--element-size={case.element_size}",
            f"--data-count={case.data_count}",
            f"--blocklen={case.blocklen}",
            f"--block-gap={case.block_gap}",
            f"--item-gap={case.item_gap}",
            f"--total-items={case.total_items}",
            f"--loop-items={case.loop_items}",
            f"--datatype-count={case.datatype_count}",
            f"--fragment-bytes={case.fragment_bytes}",
            f"--cycles={args.cycles}",
            f"--trials={args.trials}",
            f"--warmups={args.warmups}",
            f"--repetitions={args.repetitions}",
            f"--min-work-bytes={args.min_work_bytes}",
        ]
        stdout = launch(command, environment, command_log, output_log)
        signature = next(
            (record for line in stdout.splitlines() if (record := parse_record(line, "SIGNATURE"))), None
        )
        results = [record for line in stdout.splitlines() if (record := parse_record(line, "RESULT"))]
        if signature is None or args.repetitions != len(results):
            raise RuntimeError(f"unexpected output from controlled case {index}")
        if case.backend != signature.get("backend") or case.operation != signature.get("operation"):
            raise RuntimeError(f"controlled case {index} reported the wrong backend or operation")
        for result in results:
            row: dict[str, object] = asdict(case)
            row.update(
                {
                    "packed_bytes": int(signature["packed_bytes"]),
                    "cycles": int(signature["cycles"]),
                    "repetition": int(result["repetition"]),
                    "mean_seconds": float(result["mean_seconds"]),
                    "trial_stddev_pct": float(result["stddev_pct"]),
                    "min_seconds": float(result["min_seconds"]),
                    "max_seconds": float(result["max_seconds"]),
                    "bandwidth_mib_s": float(result["bandwidth_mib_s"]),
                }
            )
            rows.append(row)
    return rows


def parse_to_self(stdout: str, threshold: int) -> list[dict[str, object]]:
    """Extract pack/unpack time rows from one complete to_self corpus run."""
    datatype = "unknown"
    operation = "unknown"
    rows: list[dict[str, object]] = []
    timing = re.compile(r"^\s*(\d+)\s+([0-9.eE+-]+)\s+([0-9.eE+-]+)\s+MB/s")
    for line in stdout.splitlines():
        stripped = line.strip()
        if stripped.startswith("# Pack by hand") or stripped.startswith("# Unpack by hand"):
            # to_self also emits hand-written baseline sections whose headers
            # start with "# Pack "/"# Unpack "; tag them so their timing rows are
            # not folded into the convertor pack/unpack measurements below.
            operation = "byhand"
            continue
        if stripped.startswith("# Pack ("):
            operation = "pack"
            continue
        if stripped.startswith("# Unpack ("):
            operation = "unpack"
            continue
        match = timing.match(line)
        if match and operation in ("pack", "unpack"):
            rows.append(
                {
                    "threshold": threshold,
                    "datatype": datatype,
                    "operation": operation,
                    "length": int(match.group(1)),
                    "seconds": float(match.group(2)),
                    "bandwidth_mib_s": float(match.group(3)),
                }
            )
            continue
        if stripped and not stripped.startswith("#") and not stripped.startswith("["):
            datatype = stripped
    if not rows:
        raise RuntimeError("to_self output contained no pack/unpack timing rows")
    return rows


def run_corpus(args: argparse.Namespace, launcher: list[str], tester: Path,
               environment: dict[str, str], command_log, output_log) -> list[dict[str, object]]:
    """Run all public-MPI datatypes while varying the consolidation threshold."""
    rows: list[dict[str, object]] = []
    for index, threshold in enumerate(args.consolidation_thresholds, 1):
        print(f"corpus [{index}/{len(args.consolidation_thresholds)}] threshold={threshold}", flush=True)
        command = launcher + [
            "--mca", "ompi_datatype_consolidate_threshold", str(threshold), str(tester),
            f"--check={args.corpus_checks}", f"--data={args.corpus_data}", f"--cycles={args.cycles}",
            f"--trials={args.trials}", f"--warmups={args.warmups}",
            f"--min-work-bytes={args.min_work_bytes}",
        ]
        stdout = launch(command, environment, command_log, output_log)
        rows.extend(parse_to_self(stdout, threshold))
    return rows


def aggregate_controlled(rows: list[dict[str, object]]) -> list[dict[str, object]]:
    """Reduce in-process repetitions to medians and expose repeat-to-repeat noise."""
    grouped: dict[tuple[object, ...], list[dict[str, object]]] = {}
    keys = list(asdict(SweepCase("", "", "", 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    for row in rows:
        grouped.setdefault(tuple(row[key] for key in keys), []).append(row)
    summaries: list[dict[str, object]] = []
    for key, group in grouped.items():
        result = dict(zip(keys, key))
        means = [float(row["mean_seconds"]) for row in group]
        mean = statistics.fmean(means)
        result.update(
            {
                "packed_bytes": int(group[0]["packed_bytes"]),
                "repetitions": len(group),
                "mean_seconds": mean,
                "median_seconds": statistics.median(means),
                "repeat_stddev_pct": 100.0 * statistics.stdev(means) / mean if 1 < len(means) else 0.0,
                "mean_trial_stddev_pct": statistics.fmean(float(row["trial_stddev_pct"]) for row in group),
            }
        )
        summaries.append(result)
    return summaries


def analyze_loops(rows: list[dict[str, object]], noise_pct: float) -> dict[str, object]:
    """Find loop grouping factors and DATA sizes with repeatable aggregate gains."""
    loop_rows = [row for row in rows if "loops" == row["stage"]]
    by_shape: dict[tuple[object, ...], dict[int, float]] = {}
    for row in loop_rows:
        key = (row["operation"], row["element_size"], row["data_count"], row["blocklen"])
        by_shape.setdefault(key, {})[int(row["loop_items"])] = float(row["median_seconds"])
    details: list[dict[str, object]] = []
    for key, timings in sorted(by_shape.items()):
        baseline = timings[1]
        for loop_items, seconds in sorted(timings.items()):
            details.append(
                {
                    "operation": key[0],
                    "element_size": key[1],
                    "data_count": key[2],
                    "blocklen": key[3],
                    "data_bytes": int(key[1]) * int(key[2]) * int(key[3]),
                    "loop_items": loop_items,
                    "speedup_pct": 100.0 * (baseline / seconds - 1.0),
                }
            )

    def acceptable_score(row: dict[str, object]) -> bool:
        # A candidate is acceptable when it wins on aggregate and its individual
        # regressions stay within a small (<=10%, at least 1) tolerance band.
        return (float(row["geomean_speedup_pct"]) > noise_pct
                and int(row["regressions"]) <= max(1, int(row["samples"]) // 10))

    recommendations: dict[str, object] = {"details": details}
    # Per-operation acceptable loop factors and per-byte cutoff scores, kept so
    # the shared parameters can be derived from evidence common to both ops.
    acceptable_items: dict[str, dict[int, dict[str, object]]] = {}
    byte_scores: dict[str, dict[int, dict[str, object]]] = {}
    for operation in ("pack", "unpack"):
        candidates = sorted({int(row["loop_items"]) for row in details if row["operation"] == operation})
        scores: list[dict[str, object]] = []
        for candidate in candidates:
            samples = [float(row["speedup_pct"]) for row in details
                       if row["operation"] == operation and int(row["loop_items"]) == candidate]
            scores.append(
                {
                    "loop_items": candidate,
                    "geomean_speedup_pct": 100.0 * (geometric_mean(1.0 + value / 100.0 for value in samples) - 1.0),
                    "wins": sum(value > noise_pct for value in samples),
                    "regressions": sum(value < -noise_pct for value in samples),
                    "samples": len(samples),
                }
            )
        acceptable = [row for row in scores if acceptable_score(row)]
        best = max(acceptable, key=lambda row: float(row["geomean_speedup_pct"])) if acceptable else None
        selected = int(best["loop_items"]) if best is not None else 1
        # A largest_beneficial_data_bytes acts as a MAX_* cutoff: the optimization
        # applies to every size at or below it, so it must not span a regressing
        # smaller size.  Score each data size for the selected factor and take the
        # largest ascending-safe cutoff rather than the largest winning size.
        selected_rows = [row for row in details
                         if row["operation"] == operation and int(row["loop_items"]) == selected]
        by_bytes: list[dict[str, object]] = []
        for data_bytes in sorted({int(row["data_bytes"]) for row in selected_rows}):
            size_samples = [float(row["speedup_pct"]) for row in selected_rows
                            if int(row["data_bytes"]) == data_bytes]
            by_bytes.append(
                {
                    "data_bytes": data_bytes,
                    "geomean_speedup_pct": 100.0 * (
                        geometric_mean(1.0 + value / 100.0 for value in size_samples) - 1.0),
                    "wins": sum(value > noise_pct for value in size_samples),
                    "regressions": sum(value < -noise_pct for value in size_samples),
                    "samples": len(size_samples),
                }
            )
        recommendations[operation] = {
            "candidate_max_items": selected,
            "largest_beneficial_data_bytes": largest_safe_cutoff(by_bytes, "data_bytes", 0, noise_pct),
            "scores": scores,
        }
        acceptable_items[operation] = {int(row["loop_items"]): row for row in acceptable}
        byte_scores[operation] = {int(row["data_bytes"]): row for row in by_bytes}

    # The shared parameter governs both operations, so a factor is only safe if it
    # independently satisfies the acceptance criteria for pack *and* unpack; take
    # the ascending-safe cutoff over the combined (worst-of-both) evidence rather
    # than the smaller of two independently-best factors, which can pick a value
    # that regresses in one direction (performance is not monotonic in the factor).
    recommendations["shared_candidate_max_items"] = max(
        1, largest_safe_cutoff(_combine_scores(acceptable_items, "loop_items"), "loop_items", 1, noise_pct)
    )
    recommendations["shared_candidate_max_data_bytes"] = largest_safe_cutoff(
        _combine_scores(byte_scores, "data_bytes"), "data_bytes", 0, noise_pct
    )
    return recommendations


def _combine_scores(per_operation: dict[str, dict[int, dict[str, object]]],
                    key: str) -> list[dict[str, object]]:
    """Merge pack/unpack score rows into worst-of-both rows for shared cutoffs.

    Only values measured (and, for the loop factor, acceptable) in *both*
    operations survive; the merged geomean is the more pessimistic of the two and
    the regression/sample counts are pooled, so largest_safe_cutoff() cannot pick
    a value that is fine for one operation but harmful for the other.
    """
    pack = per_operation.get("pack", {})
    unpack = per_operation.get("unpack", {})
    combined: list[dict[str, object]] = []
    for value in sorted(set(pack) & set(unpack)):
        p, u = pack[value], unpack[value]
        combined.append(
            {
                key: value,
                "geomean_speedup_pct": min(float(p["geomean_speedup_pct"]),
                                           float(u["geomean_speedup_pct"])),
                "regressions": int(p["regressions"]) + int(u["regressions"]),
                "samples": int(p["samples"]) + int(u["samples"]),
            }
        )
    return combined


def analyze_movers(rows: list[dict[str, object]], noise_pct: float) -> dict[str, object]:
    """Compare the type-labeled interpreter with the compact reference interpreter."""
    mover_rows = [row for row in rows if "movers" == row["stage"]]
    pairs: dict[tuple[object, ...], dict[str, float]] = {}
    for row in mover_rows:
        key = (
            row["operation"], row["element_size"], row["data_count"], row["blocklen"],
            row["block_gap"], row["fragment_bytes"], row["packed_bytes"],
        )
        pairs.setdefault(key, {})[str(row["backend"])] = float(row["median_seconds"])
    details: list[dict[str, object]] = []
    for key, timings in sorted(pairs.items()):
        if set(timings) != {"current", "reference"}:
            raise RuntimeError(f"incomplete current/reference pair: {key}")
        delta = 100.0 * (timings["reference"] / timings["current"] - 1.0)
        details.append(
            {
                "operation": key[0],
                "element_size": key[1],
                "data_count": key[2],
                "blocklen": key[3],
                "block_bytes": int(key[1]) * int(key[3]),
                "block_gap": key[4],
                "fragment_bytes": key[5],
                "packed_bytes": key[6],
                "current_seconds": timings["current"],
                "reference_seconds": timings["reference"],
                "current_speedup_pct": delta,
                "classification": "current" if delta > noise_pct else "reference" if delta < -noise_pct else "noise",
            }
        )

    analysis: dict[str, object] = {"details": details}
    for operation in ("pack", "unpack"):
        operation_rows = [row for row in details if row["operation"] == operation]
        by_block: list[dict[str, object]] = []
        for blocklen in sorted({int(row["blocklen"]) for row in operation_rows}):
            samples = [float(row["current_speedup_pct"]) for row in operation_rows
                       if int(row["blocklen"]) == blocklen]
            by_block.append(
                {
                    "blocklen": blocklen,
                    "geomean_speedup_pct": 100.0 * (geometric_mean(1.0 + value / 100.0 for value in samples) - 1.0),
                    "wins": sum(value > noise_pct for value in samples),
                    "regressions": sum(value < -noise_pct for value in samples),
                    "samples": len(samples),
                }
            )
        analysis[operation] = {
            "largest_consistently_beneficial_blocklen":
                largest_safe_cutoff(by_block, "blocklen", 8, noise_pct),
            "by_blocklen": by_block,
        }
        by_bytes: list[dict[str, object]] = []
        for block_bytes in sorted({int(row["block_bytes"]) for row in operation_rows}):
            samples = [float(row["current_speedup_pct"]) for row in operation_rows
                       if int(row["block_bytes"]) == block_bytes]
            by_bytes.append(
                {
                    "block_bytes": block_bytes,
                    "geomean_speedup_pct": 100.0 * (
                        geometric_mean(1.0 + value / 100.0 for value in samples) - 1.0
                    ),
                    "wins": sum(value > noise_pct for value in samples),
                    "regressions": sum(value < -noise_pct for value in samples),
                    "samples": len(samples),
                }
            )
        analysis[operation]["largest_consistently_beneficial_block_bytes"] = (
            largest_safe_cutoff(by_bytes, "block_bytes", 32, noise_pct)
        )
        analysis[operation]["by_block_bytes"] = by_bytes
    return analysis


def analyze_corpus(rows: list[dict[str, object]], thresholds: list[int], noise_pct: float) -> dict[str, object]:
    """Compare consolidation thresholds over matching public-MPI corpus points."""
    if not rows:
        return {}
    baseline_threshold = 250 if 250 in thresholds else thresholds[0]
    indexed: dict[int, dict[tuple[object, ...], float]] = {}
    for row in rows:
        key = (row["datatype"], row["operation"], row["length"])
        indexed.setdefault(int(row["threshold"]), {})[key] = float(row["seconds"])
    # The baseline threshold may have produced no usable rows (all filtered, or
    # the corpus never exercised it); without it there is nothing to compare.
    baseline = indexed.get(baseline_threshold)
    if not baseline:
        return {}
    summary: list[dict[str, object]] = []
    for threshold in thresholds:
        common = sorted(set(baseline) & set(indexed.get(threshold, {})))
        # Skip points whose baseline or candidate time is non-positive/non-finite:
        # they would divide by zero or feed a non-positive ratio into the
        # geometric mean (which rejects such values).
        candidate = indexed.get(threshold, {})
        ratios = [
            candidate[key] / baseline[key]
            for key in common
            if math.isfinite(baseline[key]) and baseline[key] > 0.0
            and math.isfinite(candidate[key]) and candidate[key] > 0.0
        ]
        summary.append(
            {
                "threshold": threshold,
                "points": len(ratios),
                # No comparable points means no measured difference from baseline.
                "time_vs_baseline_pct": 100.0 * (geometric_mean(ratios) - 1.0) if ratios else 0.0,
            }
        )
    fastest = min(summary, key=lambda row: float(row["time_vs_baseline_pct"]))
    selected = fastest if float(fastest["time_vs_baseline_pct"]) < -noise_pct else {
        "threshold": baseline_threshold
    }
    return {
        "baseline_threshold": baseline_threshold,
        "candidate_threshold": int(selected["threshold"]),
        "summary": summary,
    }


def write_tsv(path: Path, rows: list[dict[str, object]]) -> None:
    """Write homogeneous dictionaries as a tab-delimited result table."""
    with path.open("w", newline="") as stream:
        if not rows:
            return
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def write_report(path: Path, manifest: dict[str, object], recommendations: dict[str, object]) -> None:
    """Render high-signal candidate values and the evidence caveat for human review."""
    lines = [
        "# Open MPI datatype tuning report",
        "",
        f"Host: `{manifest['hostname']}`; processor: `{manifest['processor']}`; profile: "
        f"`{manifest['profile']}`.",
        "",
        "Candidate values are architecture-specific measurements, not automatic source edits. A value should be "
        "validated with the full datatype corpus before changing production policy.",
        "",
    ]
    parameters = recommendations.get("candidate_parameters", {})
    if parameters:
        lines.extend(["## Candidate parameters", "", "| Parameter | Candidate |", "|---|---:|"])
        for name, value in parameters.items():
            lines.append(f"| `{name}` | {value} |")
        lines.append("")
    corpus = recommendations.get("consolidation", {})
    if corpus:
        lines.extend(["## Consolidation", "", "| Threshold | Geomean time vs baseline | Points |",
                      "|---:|---:|---:|"])
        for row in corpus["summary"]:
            lines.append(f"| {row['threshold']} | {row['time_vs_baseline_pct']:+.2f}% | {row['points']} |")
        lines.extend(["", f"Candidate: `{corpus['candidate_threshold']}`.", ""])
    loops = recommendations.get("loops", {})
    if loops:
        lines.extend(["## Loop unrolling", "", "| Operation | Candidate max items | Largest beneficial DATA |",
                      "|---|---:|---:|"])
        for operation in ("pack", "unpack"):
            row = loops[operation]
            lines.append(
                f"| {operation} | {row['candidate_max_items']} | {row['largest_beneficial_data_bytes']} bytes |"
            )
        lines.extend(["", f"Conservative shared candidate: `{loops['shared_candidate_max_items']}` items.", ""])
    movers = recommendations.get("movers", {})
    if movers:
        lines.extend(["## Type-labeled mover", "", "| Operation | Largest consistently beneficial blocklen |",
                      "|---|---:|"])
        for operation in ("pack", "unpack"):
            lines.append(f"| {operation} | {movers[operation]['largest_consistently_beneficial_blocklen']} |")
        lines.extend(
            ["", "Positive percentages in `mover_details.tsv` mean the current type-labeled interpreter is faster. "
             "Rows within the configured noise threshold are classified as noise.", ""]
        )
        lines.extend(
            ["The current/reference experiment can validate an existing selector but cannot force a typed mover "
             "after that selector has chosen the reference path. Fragment and sparse-extent boundaries therefore "
             "remain evidence tables rather than automatic candidate constants.", ""]
        )
    path.write_text("\n".join(lines) + "\n")


def ensure_outputs(args: argparse.Namespace) -> None:
    """Refuse accidental data replacement unless the caller explicitly requested it."""
    result_names = (
        "manifest.json", "run_config.json", "commands.log", "program-output.log", "corpus_raw.tsv",
        "controlled_raw.tsv", "controlled_summary.tsv", "loop_details.tsv", "mover_details.tsv",
        "recommendations.json", "report.md",
    )
    args.output.mkdir(parents=True, exist_ok=True)
    existing = [name for name in result_names if (args.output / name).exists()]
    if existing and not args.force:
        raise RuntimeError(f"result files already exist under {args.output}; use --force to overwrite")


def main() -> int:
    """Resolve one build, run requested stages through mpirun, and analyze the measurements."""
    args = parse_args()
    build_dir = args.build_dir.expanduser().resolve()
    mpirun = resolve_executable(args.mpirun, configured_mpirun(build_dir), "mpirun", "--mpirun")
    to_self = resolve_executable(
        args.to_self, build_dir / "ompi" / "test" / "datatype" / "to_self", "to_self", "--to-self"
    )
    sweep = resolve_executable(
        args.description_sweep, build_dir / "ompi" / "test" / "datatype" / "pack_description_sweep",
        "pack_description_sweep", "--description-sweep"
    )
    ensure_outputs(args)

    launcher = [str(mpirun), "-n", "1", *args.mpirun_arg]
    environment = os.environ.copy()
    environment.setdefault("OMPI_MCA_btl", "self")
    cache = cache_description()
    version = subprocess.run([str(mpirun), "--version"], text=True, capture_output=True)
    manifest: dict[str, object] = {
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "hostname": platform.node(),
        "platform": platform.platform(),
        "machine": platform.machine(),
        "processor": cpu_description(),
        "cache": cache,
        "profile": args.profile,
        "checks": args.check,
        "mpirun": str(mpirun),
        "mpirun_version": (version.stdout or version.stderr).strip(),
        "to_self": str(to_self),
        "to_self_sha256": sha256(to_self),
        "pack_description_sweep": str(sweep),
        "pack_description_sweep_sha256": sha256(sweep),
    }
    config = {
        "build_dir": str(build_dir),
        "launcher": launcher,
        "cycles": args.cycles,
        "trials": args.trials,
        "warmups": args.warmups,
        "repetitions": args.repetitions,
        "min_work_bytes": args.min_work_bytes,
        "noise_pct": args.noise_pct,
        "consolidation_thresholds": args.consolidation_thresholds,
        "corpus_checks": args.corpus_checks,
        "corpus_data": args.corpus_data,
    }
    (args.output / "manifest.json").write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
    (args.output / "run_config.json").write_text(json.dumps(config, indent=2, sort_keys=True) + "\n")

    corpus_rows: list[dict[str, object]] = []
    controlled_rows: list[dict[str, object]] = []
    with (args.output / "commands.log").open("w") as command_log, (
        args.output / "program-output.log"
    ).open("w") as output_log:
        if "corpus" in args.check:
            corpus_rows = run_corpus(args, launcher, to_self, environment, command_log, output_log)
        if any(stage in args.check for stage in ("loops", "movers")):
            controlled_rows = run_controlled_cases(
                args, launcher, sweep, environment, command_log, output_log
            )

    summaries = aggregate_controlled(controlled_rows) if controlled_rows else []
    recommendations: dict[str, object] = {"noise_pct": args.noise_pct}
    if corpus_rows:
        recommendations["consolidation"] = analyze_corpus(
            corpus_rows, args.consolidation_thresholds, args.noise_pct
        )
    if "loops" in args.check:
        recommendations["loops"] = analyze_loops(summaries, args.noise_pct)
    if "movers" in args.check:
        recommendations["movers"] = analyze_movers(summaries, args.noise_pct)

    parameters: dict[str, int] = {}
    # analyze_corpus() returns an empty mapping when there was nothing comparable
    # to measure; only emit the parameter when a candidate was actually chosen.
    if recommendations.get("consolidation"):
        parameters["ompi_datatype_consolidate_threshold"] = int(
            recommendations["consolidation"]["candidate_threshold"]
        )
    # These are the registered runtime MCA parameter names (see the threshold
    # registration block in opal/datatype/opal_datatype_module.c); emit them
    # verbatim so a user can paste them straight into --mca.
    if "loops" in recommendations:
        parameters["opal_datatype_optimize_loop_unroll_max_items"] = int(
            recommendations["loops"]["shared_candidate_max_items"]
        )
        max_data_bytes = int(
            recommendations["loops"]["shared_candidate_max_data_bytes"]
        )
        if 0 < max_data_bytes:
            parameters["opal_datatype_optimize_loop_unroll_max_data_bytes"] = max_data_bytes
    if "movers" in recommendations:
        pack_blocklen = int(
            recommendations["movers"]["pack"]["largest_consistently_beneficial_blocklen"]
        )
        unpack_block_bytes = int(
            recommendations["movers"]["unpack"]["largest_consistently_beneficial_block_bytes"]
        )
        if 0 < pack_blocklen:
            parameters["opal_datatype_pack_max_vectorized_blocklen"] = pack_blocklen
        if 0 < unpack_block_bytes:
            parameters["opal_datatype_unpack_max_vectorized_block_bytes"] = unpack_block_bytes
    recommendations["candidate_parameters"] = parameters

    write_tsv(args.output / "corpus_raw.tsv", corpus_rows)
    write_tsv(args.output / "controlled_raw.tsv", controlled_rows)
    write_tsv(args.output / "controlled_summary.tsv", summaries)
    write_tsv(
        args.output / "loop_details.tsv",
        recommendations["loops"]["details"] if "loops" in recommendations else [],
    )
    write_tsv(
        args.output / "mover_details.tsv",
        recommendations["movers"]["details"] if "movers" in recommendations else [],
    )
    (args.output / "recommendations.json").write_text(
        json.dumps(recommendations, indent=2, sort_keys=True) + "\n"
    )
    write_report(args.output / "report.md", manifest, recommendations)
    print(f"Results: {args.output.resolve()}")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError, subprocess.SubprocessError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        sys.exit(1)
