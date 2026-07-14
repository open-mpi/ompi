#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

"""Compute performance regressions between two saved to_self comparison runs.

The to_self runner (run_to_self_pack_comparison.py) already compares
implementations that are launched side by side in a single collection.  This
tool instead compares two *previously collected* result directories -- for
example the same benchmark run before and after a code change -- and reports,
per implementation/datatype/size, whether the candidate run regressed relative
to the baseline run.

It consumes each run's per-trial samples (trials.tsv), so the comparison uses
the raw measurements rather than pre-reduced medians.  Because the two runs are
independent (not trial-paired), the estimator is an unpaired ratio of medians
with a deterministic percentile bootstrap confidence interval, mirroring the
runner's bootstrap so the numbers are directly comparable.

The output is written in the exact schema the runner emits (summary.tsv plus
datatype_summary.tsv), so plot_to_self_pack_comparison.py plots it unchanged:
a positive speedup_pct means the candidate is faster than the baseline; a
negative value is a regression.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import random
import shutil
import statistics
import subprocess
import sys
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path

# Reuse the runner's bootstrap sample count so the confidence intervals here are
# produced with the same resolution and determinism as the collection tool.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from run_to_self_pack_comparison import BOOTSTRAP_SAMPLES  # noqa: E402

try:  # numpy is optional, but it makes the bootstrap dramatically faster.
    import numpy as _np
except ImportError:
    _np = None


# run_config.json controls that must match for a comparison to be meaningful: a
# difference in any of these means the two runs measured different work.
CONTROL_KEYS = (
    "scenario", "cycles", "trials", "warmups", "min_work_bytes", "data", "send", "recv",
    "processes", "command_prefix", "launcher_args", "preserve_full_order",
)
# Defaults for controls a run may legitimately omit, so that a run_config.json
# written before a key existed compares equal to a newer run that stored the
# key at its default rather than being flagged as an incompatible difference.
CONTROL_DEFAULTS = {
    "command_prefix": "",
    "launcher_args": [],
    "preserve_full_order": False,
}
# manifest.txt fields describing the machine the run executed on.
ENVIRONMENT_KEYS = ("hostname", "machine", "processor")


def parse_args() -> argparse.Namespace:
    directory = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--baseline-run", type=Path, required=True,
                        help="result directory of the reference run")
    parser.add_argument("--candidate-run", type=Path, required=True,
                        help="result directory of the run being checked for regressions")
    parser.add_argument("--output", type=Path, required=True, help="regression output directory")
    parser.add_argument("--baseline-label", default=None,
                        help="label for the reference run (default: its directory name)")
    parser.add_argument("--candidate-label", default=None,
                        help="label for the candidate run (default: its directory name)")
    parser.add_argument("--implementation", action="append", default=None, metavar="KEY",
                        help="compare only this implementation key (repeatable); "
                             "use 'list' to print the keys common to both runs and exit")
    parser.add_argument("--all-implementations", action="store_true",
                        help="compare every implementation common to both runs "
                             "(default: just one, preferring 'base')")
    parser.add_argument("--operation", default=None, metavar="OP",
                        help="compare only this operation (pack, unpack, isend_recv, ...); "
                             "use 'list' to print the operations common to both runs and exit. "
                             "A single comparison covers one operation (the plotter refuses to "
                             "mix them); the default is the sole operation present, preferring "
                             "'pack'.")
    parser.add_argument("--noise", type=float, default=3.0,
                        help="+/- percent band treated as noise when counting regressions")
    parser.add_argument("--bootstrap", type=int, default=BOOTSTRAP_SAMPLES,
                        help="bootstrap resamples per point (lower is faster but less precise)")
    parser.add_argument("--allow-mismatch", action="store_true",
                        help="downgrade differing controls/environment from an error to a warning")
    parser.add_argument("--force", action="store_true", help="overwrite an existing output directory")
    parser.add_argument("--no-plot", action="store_true", help="do not invoke the plotter")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="print per-point progress to stderr")
    parser.add_argument("--plotter", type=Path, default=directory / "plot_to_self_pack_comparison.py",
                        help="plotter used to render the regression summary")
    args = parser.parse_args()
    if args.noise < 0.0:
        parser.error("--noise cannot be negative")
    if args.bootstrap < 1:
        parser.error("--bootstrap must be positive")
    return args


def read_manifest(run_dir: Path) -> dict[str, str]:
    """Parse the key=value manifest.txt written by the runner."""
    manifest: dict[str, str] = {}
    path = run_dir / "manifest.txt"
    if not path.is_file():
        return manifest
    for line in path.read_text().splitlines():
        if "=" in line:
            key, _, value = line.partition("=")
            manifest[key] = value
    return manifest


def read_run_config(run_dir: Path) -> dict[str, object]:
    """Parse run_config.json; return an empty mapping when it is absent."""
    path = run_dir / "run_config.json"
    if not path.is_file():
        return {}
    return json.loads(path.read_text())


def guard_comparable(
    baseline_dir: Path, candidate_dir: Path, allow_mismatch: bool,
) -> list[str]:
    """Fail (or warn) when the two runs are not measuring comparable work.

    Binary identity is deliberately *not* required: a regression check expects
    two different executables.  Only the measured work (run_config controls) and
    the machine (manifest environment) need to agree.
    """
    warnings: list[str] = []
    problems: list[str] = []
    baseline_config = read_run_config(baseline_dir)
    candidate_config = read_run_config(candidate_dir)
    for key in CONTROL_KEYS:
        default = CONTROL_DEFAULTS.get(key)
        baseline_value = baseline_config.get(key, default)
        candidate_value = candidate_config.get(key, default)
        if baseline_value != candidate_value:
            problems.append(
                f"control '{key}' differs: baseline={baseline_value!r} "
                f"candidate={candidate_value!r}"
            )
    baseline_manifest = read_manifest(baseline_dir)
    candidate_manifest = read_manifest(candidate_dir)
    for key in ENVIRONMENT_KEYS:
        if baseline_manifest.get(key) != candidate_manifest.get(key):
            warnings.append(
                f"environment '{key}' differs: baseline={baseline_manifest.get(key)!r} "
                f"candidate={candidate_manifest.get(key)!r}"
            )
    if problems and not allow_mismatch:
        raise RuntimeError(
            "runs are not comparable (use --allow-mismatch to override):\n  "
            + "\n  ".join(problems)
        )
    return warnings + problems


def load_trials(
    run_dir: Path,
) -> tuple[
    dict[tuple[str, str, int, str, int, str], list[float]],
    dict[str, str],
    set[str],
    dict[str, tuple[int, str]],
    dict[tuple[str, str, int, str, int, str], set[str]],
]:
    """Load per-trial samples from a run's trials.tsv.

    Returns the sample pools keyed by (implementation_key, operation,
    datatype_index, datatype, size_bytes, statistic) with statistic in
    {filtered, unfiltered}; a map of implementation_key -> label; the set of
    operations present in the file; datatype name -> (index, name); and, keyed
    the same way as the sample pools, the set of distinct repetition ids that
    contributed samples to each pool.

    Each pool holds every *trial* sample (a repetition contributes several
    trials), so len(pool) is not the number of independent runs.  The repetition
    id sets let callers report the count of independent repetitions rather than
    the far larger pooled trial count.

    Operation is part of the sample key so a trials.tsv holding more than one
    operation (for example a pack run that also records a "pack_byhand" series,
    or a combined --check=all collection) never mixes samples across operations.
    """
    path = run_dir / "trials.tsv"
    if not path.is_file():
        raise RuntimeError(f"{run_dir} has no trials.tsv; rerun the collector to keep raw trials")
    samples: dict[tuple[str, str, int, str, int, str], list[float]] = defaultdict(list)
    repetitions: dict[tuple[str, str, int, str, int, str], set[str]] = defaultdict(set)
    labels: dict[str, str] = {}
    operations: set[str] = set()
    datatypes: dict[str, tuple[int, str]] = {}
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream, delimiter="\t")
        required = {
            "datatype_index", "datatype", "implementation_key", "implementation_label",
            "operation", "size_bytes", "seconds", "retained",
        }
        if reader.fieldnames is None or not required.issubset(reader.fieldnames):
            raise RuntimeError(f"{path} does not contain the expected trial columns")
        # The repetition column is optional: fall back to a per-row identity so a
        # trials.tsv written before the column existed still yields a repetition
        # count (one repetition per trial) rather than crashing.
        has_repetition = "repetition" in reader.fieldnames
        for line_no, row in enumerate(reader):
            seconds = float(row["seconds"])
            if not math.isfinite(seconds) or seconds <= 0.0:
                raise RuntimeError(f"{path} contains a non-positive trial time: {seconds}")
            impl_key = row["implementation_key"]
            operation = row["operation"]
            index = int(row["datatype_index"])
            name = row["datatype"]
            size = int(row["size_bytes"])
            rep = row["repetition"] if has_repetition else str(line_no)
            labels[impl_key] = row["implementation_label"]
            operations.add(operation)
            datatypes.setdefault(name, (index, name))
            unfiltered_key = (impl_key, operation, index, name, size, "unfiltered")
            samples[unfiltered_key].append(seconds)
            repetitions[unfiltered_key].add(rep)
            if int(row["retained"]):
                filtered_key = (impl_key, operation, index, name, size, "filtered")
                samples[filtered_key].append(seconds)
                repetitions[filtered_key].add(rep)
    if not samples:
        raise RuntimeError(f"{path} contains no trials")
    return samples, labels, operations, datatypes, repetitions


def _fast_median(values: list[float]) -> float:
    """Median of a list, cheaper than statistics.median in the bootstrap hot loop."""
    ordered = sorted(values)
    n = len(ordered)
    mid = n // 2
    if n & 1:
        return ordered[mid]
    return 0.5 * (ordered[mid - 1] + ordered[mid])


def _bootstrap_medians_numpy(generator, pool: list[float], samples: int):
    """Medians of `samples` bootstrap resamples of `pool`, vectorized with numpy.

    Uses np.partition rather than a full sort per resample: only the middle
    order statistic(s) are needed, which is what median reduces to.
    """
    array = _np.asarray(pool, dtype=float)
    n = array.size
    draws = array[generator.integers(0, n, size=(samples, n))]
    mid = n // 2
    if n & 1:
        return _np.partition(draws, mid, axis=1)[:, mid]
    partitioned = _np.partition(draws, (mid - 1, mid), axis=1)
    return 0.5 * (partitioned[:, mid - 1] + partitioned[:, mid])


def bootstrap_ratio_ci(
    subject: list[float], baseline: list[float], identity: str, samples: int,
) -> tuple[float, float, float, float]:
    """Unpaired ratio-of-medians speedup and a deterministic 95% bootstrap CI.

    The point estimate is 100 * (median(baseline) / median(subject) - 1): it is
    positive when the subject (candidate) is faster than the baseline.  The two
    runs are independent, so each bootstrap iteration resamples the subject and
    baseline pools separately.  The seed is derived from a stable identity so
    repeated invocations produce identical intervals.

    When numpy is available the resampling is vectorized (orders of magnitude
    faster); otherwise a pure-Python fallback produces an equally valid interval.
    The two backends are each deterministic but do not produce bit-identical
    intervals, since they draw from different RNGs.
    """
    subject_median = statistics.median(subject)
    baseline_median = statistics.median(baseline)
    point = 100.0 * (baseline_median / subject_median - 1.0)
    if 1 == len(subject) and 1 == len(baseline):
        return point, point, point, 0.0
    seed = int.from_bytes(hashlib.sha256(identity.encode()).digest()[:8], "big")
    low_index = int(0.025 * (samples - 1))
    high_index = int(0.975 * (samples - 1))
    if _np is not None:
        generator = _np.random.default_rng(seed)
        subject_medians = _bootstrap_medians_numpy(generator, subject, samples)
        baseline_medians = _bootstrap_medians_numpy(generator, baseline, samples)
        resampled = 100.0 * (baseline_medians / subject_medians - 1.0)
        resampled.sort()
        return (
            point, float(resampled[low_index]), float(resampled[high_index]),
            float(_np.median(_np.abs(resampled - point))),
        )
    generator = random.Random(seed)
    subject_k = len(subject)
    baseline_k = len(baseline)
    resampled = sorted(
        100.0 * (
            _fast_median(generator.choices(baseline, k=baseline_k))
            / _fast_median(generator.choices(subject, k=subject_k))
            - 1.0
        )
        for _ in range(samples)
    )
    low = resampled[low_index]
    high = resampled[high_index]
    mad = _fast_median([abs(value - point) for value in resampled])
    return point, low, high, mad


SUMMARY_FIELDS = [
    "datatype_index", "datatype", "operation", "size_bytes", "statistic", "comparison_key",
    "comparison_label", "subject_key", "subject_label", "subject_seconds", "baseline_key",
    "baseline_label", "baseline_seconds", "independent_speedup_pct", "speedup_pct",
    "ci_low_pct", "ci_high_pct", "mad_pct", "repetitions",
]
AGGREGATE_FIELDS = [
    "datatype_index", "datatype", "operation", "statistic", "comparison_key", "comparison_label",
    "size_points", "geomean_pct", "worst_pct", "best_pct", "faster_points", "slower_points",
    "noise_points",
]


def write_summary(path: Path, rows: list[dict[str, object]]) -> None:
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=SUMMARY_FIELDS, delimiter="\t",
                                lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def write_aggregate(path: Path, rows: list[dict[str, object]], noise: float) -> None:
    """Roll each comparison up per datatype, mirroring the runner's datatype_summary.tsv."""
    grouped: dict[tuple[int, str, str, str, str], list[dict[str, object]]] = defaultdict(list)
    for row in rows:
        key = (
            int(row["datatype_index"]), str(row["datatype"]), str(row["operation"]),
            str(row["statistic"]), str(row["comparison_key"]),
        )
        grouped[key].append(row)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=AGGREGATE_FIELDS, delimiter="\t",
                                lineterminator="\n")
        writer.writeheader()
        for (index, datatype, operation, statistic, comparison_key), comparison_rows in sorted(
            grouped.items()
        ):
            values = [float(row["speedup_pct"]) for row in comparison_rows]
            ratios = [1.0 + value / 100.0 for value in values]
            writer.writerow(
                {
                    "datatype_index": index,
                    "datatype": datatype,
                    "operation": operation,
                    "statistic": statistic,
                    "comparison_key": comparison_key,
                    "comparison_label": comparison_rows[0]["comparison_label"],
                    "size_points": len(comparison_rows),
                    "geomean_pct":
                        f"{100.0 * (math.exp(statistics.fmean(map(math.log, ratios))) - 1.0):.6f}",
                    "worst_pct": f"{min(values):.6f}",
                    "best_pct": f"{max(values):.6f}",
                    "faster_points": sum(value > noise for value in values),
                    "slower_points": sum(value < -noise for value in values),
                    "noise_points": sum(-noise <= value <= noise for value in values),
                }
            )


def main() -> int:
    args = parse_args()
    args.output = args.output.expanduser().resolve()
    baseline_dir = args.baseline_run.expanduser().resolve()
    candidate_dir = args.candidate_run.expanduser().resolve()
    baseline_label = args.baseline_label or baseline_dir.name
    candidate_label = args.candidate_label or candidate_dir.name

    listing_impls = args.implementation is not None and "list" in args.implementation
    listing_ops = "list" == args.operation
    listing = listing_impls or listing_ops

    summary_path = args.output / "summary.tsv"
    if not listing:
        if summary_path.exists() and not args.force:
            raise RuntimeError(f"{summary_path} exists; use --force or a new output directory")
        args.output.mkdir(parents=True, exist_ok=True)
        # --force reuses the output directory; drop the previous run's generated
        # graphs so a different operation/implementation/datatype selection cannot
        # leave stale plots masquerading as part of the current comparison.
        if args.force:
            stale_graphs = args.output / "graphs"
            if stale_graphs.exists():
                shutil.rmtree(stale_graphs)

        notices = guard_comparable(baseline_dir, candidate_dir, args.allow_mismatch)
        for notice in notices:
            print(f"warning: {notice}", file=sys.stderr)
    else:
        notices = []

    baseline_samples, baseline_labels, baseline_operations, _, baseline_reps = load_trials(
        baseline_dir
    )
    (candidate_samples, candidate_labels, candidate_operations, candidate_datatypes,
     candidate_reps) = load_trials(candidate_dir)

    common_ops = sorted(baseline_operations & candidate_operations)
    if not common_ops:
        raise RuntimeError(
            "the two runs share no operation: baseline has "
            f"{sorted(baseline_operations)}, candidate has {sorted(candidate_operations)}"
        )
    if listing_ops:
        print("operations common to both runs:")
        for op in common_ops:
            print(f"  {op}")
        return 0
    if args.operation is not None:
        if args.operation not in common_ops:
            raise RuntimeError(
                f"operation {args.operation!r} not present in both runs; "
                f"available: {', '.join(common_ops)}"
            )
        operation = args.operation
    elif 1 == len(common_ops):
        operation = common_ops[0]
    else:
        # A comparison covers one operation (the plotter refuses to mix them); pick a
        # sensible default rather than silently choosing an arbitrary one.
        operation = "pack" if "pack" in common_ops else common_ops[0]
        print(f"note: runs contain operations {', '.join(common_ops)}; comparing only "
              f"'{operation}'. Use --operation OP to pick another (or 'list' to enumerate).",
              file=sys.stderr)

    # Restrict the implementations to those that actually recorded the chosen operation
    # in both runs (for example a "*_byhand" series only exists for the hand executable).
    baseline_op_impls = {impl for (impl, op, *_rest) in baseline_samples if op == operation}
    candidate_op_impls = {impl for (impl, op, *_rest) in candidate_samples if op == operation}
    common_impls = sorted(baseline_op_impls & candidate_op_impls)
    if not common_impls:
        raise RuntimeError(f"the two runs share no implementation for operation {operation!r}")
    if listing_impls:
        print(f"implementations common to both runs for operation '{operation}':")
        for impl_key in common_impls:
            print(f"  {impl_key}\t{candidate_labels[impl_key]}")
        return 0
    if args.implementation is not None:
        requested = set(args.implementation)
        missing = requested - set(common_impls)
        if missing:
            raise RuntimeError(f"implementation(s) not present in both runs: {', '.join(sorted(missing))}")
        common_impls = [impl for impl in common_impls if impl in requested]
    elif not args.all_implementations and 1 < len(common_impls):
        # A regression check usually cares about one implementation; pick a single
        # sensible default rather than plotting every version at once.
        chosen = "base" if "base" in common_impls else common_impls[0]
        print(f"note: runs contain implementations {', '.join(common_impls)}; comparing only "
              f"'{chosen}'. Use --implementation KEY to pick another, or --all-implementations "
              f"for all.", file=sys.stderr)
        common_impls = [chosen]

    def op_keys(samples: dict, impl: str) -> set:
        return {
            (index, name, size, statistic)
            for (s_impl, s_op, index, name, size, statistic) in samples
            if s_impl == impl and s_op == operation
        }

    # Compare only the points present in *both* runs, but detect and record the
    # points present in only one run (in either direction) so a candidate that
    # lost datatypes/sizes -- or gained new ones absent from the baseline -- is
    # surfaced instead of being silently dropped.
    impl_keys: dict[str, list] = {}
    skipped: list[dict[str, object]] = []
    for impl_key in common_impls:
        candidate_keys = op_keys(candidate_samples, impl_key)
        baseline_keys = op_keys(baseline_samples, impl_key)
        impl_keys[impl_key] = sorted(candidate_keys & baseline_keys)
        # Account for missing points at (index, name, size) granularity using the
        # always-present "unfiltered" statistic, so each point is reported once
        # (not once per statistic) together with the run it is missing from.
        candidate_points = {(i, n, s) for (i, n, s, stat) in candidate_keys if "unfiltered" == stat}
        baseline_points = {(i, n, s) for (i, n, s, stat) in baseline_keys if "unfiltered" == stat}
        for index, name, size in sorted(candidate_points - baseline_points):
            skipped.append({"implementation": impl_key, "datatype_index": index, "datatype": name,
                            "size_bytes": size, "missing_from": "baseline",
                            "missing_from_label": baseline_label})
        for index, name, size in sorted(baseline_points - candidate_points):
            skipped.append({"implementation": impl_key, "datatype_index": index, "datatype": name,
                            "size_bytes": size, "missing_from": "candidate",
                            "missing_from_label": candidate_label})
    total_points = sum(len(keys) for keys in impl_keys.values())
    if args.verbose:
        backend = "numpy" if _np is not None else "pure-python"
        print(f"comparing {len(common_impls)} implementation(s), {total_points} point(s), "
              f"{args.bootstrap} bootstrap resamples ({backend})", file=sys.stderr, flush=True)
    interactive = sys.stderr.isatty()
    processed = 0
    reported_pct = -1

    rows: list[dict[str, object]] = []
    for impl_key in common_impls:
        # Comparison key names the implementation so multiple implementations plot
        # as distinct series; the label records which run is baseline vs candidate.
        comparison_key = f"{impl_key}_regression"
        comparison_label = (
            f"{candidate_labels[impl_key]}: {candidate_label} vs {baseline_label}"
        )
        for index, name, size, statistic in impl_keys[impl_key]:
            processed += 1
            if args.verbose:
                if interactive:
                    print(f"\r[{processed}/{total_points}] {impl_key} {name}@{size} "
                          f"({statistic})    ", end="", file=sys.stderr, flush=True)
                else:
                    pct = processed * 100 // total_points
                    if pct >= reported_pct + 5:
                        reported_pct = pct
                        print(f"[{processed}/{total_points}] {pct}%", file=sys.stderr, flush=True)
            key = (impl_key, operation, index, name, size, statistic)
            subject = candidate_samples.get(key, [])
            baseline = baseline_samples.get(key, [])
            if not subject or not baseline:
                # impl_keys is the intersection, so this only guards a statistic
                # that exists in one run but not the other; the point-level miss
                # was already recorded above.
                continue
            identity = f"{impl_key}:{operation}:{index}:{name}:{size}:{statistic}"
            point, low, high, mad = bootstrap_ratio_ci(subject, baseline, identity, args.bootstrap)
            rows.append(
                {
                    "datatype_index": index,
                    "datatype": name,
                    "operation": operation,
                    "size_bytes": size,
                    "statistic": statistic,
                    "comparison_key": comparison_key,
                    "comparison_label": comparison_label,
                    "subject_key": impl_key,
                    "subject_label": f"{candidate_labels[impl_key]} ({candidate_label})",
                    "subject_seconds": f"{statistics.median(subject):.12g}",
                    "baseline_key": impl_key,
                    "baseline_label": f"{baseline_labels[impl_key]} ({baseline_label})",
                    "baseline_seconds": f"{statistics.median(baseline):.12g}",
                    "independent_speedup_pct": f"{point:.6f}",
                    "speedup_pct": f"{point:.6f}",
                    "ci_low_pct": f"{low:.6f}",
                    "ci_high_pct": f"{high:.6f}",
                    "mad_pct": f"{mad:.6f}",
                    "repetitions": min(len(candidate_reps.get(key, ())),
                                       len(baseline_reps.get(key, ()))),
                }
            )

    if args.verbose and interactive:
        print(file=sys.stderr)
    if not rows:
        raise RuntimeError("the two runs share no comparable datatype/size points")

    # Stable ordering matching the runner: by datatype index then size.
    rows.sort(key=lambda row: (int(row["datatype_index"]), row["comparison_key"],
                               row["statistic"], int(row["size_bytes"])))
    write_summary(summary_path, rows)
    write_aggregate(args.output / "datatype_summary.tsv", rows, args.noise)

    config = {
        "baseline_run": str(baseline_dir),
        "candidate_run": str(candidate_dir),
        "baseline_label": baseline_label,
        "candidate_label": candidate_label,
        "operation": operation,
        "implementations": common_impls,
        "noise_pct": args.noise,
        "allow_mismatch": args.allow_mismatch,
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "notices": notices,
        "skipped_points": skipped,
    }
    (args.output / "comparison_config.json").write_text(
        json.dumps(config, indent=2, sort_keys=True) + "\n"
    )

    _report_console(rows, args.noise, candidate_datatypes)
    if skipped:
        print(f"note: {len(skipped)} point(s) present in only one run were skipped "
              f"(also recorded in comparison_config.json):", file=sys.stderr)
        for entry in skipped:
            print(f"  {entry['implementation']} {entry['datatype']}@{entry['size_bytes']} "
                  f"missing from the {entry['missing_from']} run "
                  f"({entry['missing_from_label']})", file=sys.stderr)

    if not args.no_plot:
        command = [
            sys.executable, str(args.plotter.expanduser().resolve()), "--input", str(summary_path),
            "--output", str(args.output / "graphs"),
            "--noise", str(args.noise), "--regression-only",
        ]
        subprocess.run(command, check=True)
    print(f"Regression results: {args.output}")
    return 0


def _report_console(
    rows: list[dict[str, object]], noise: float, datatypes: dict[str, tuple[int, str]],
) -> None:
    """Print a compact per-implementation regression summary to stdout."""
    filtered = [row for row in rows if "filtered" == row["statistic"]]
    by_impl: dict[str, list[dict[str, object]]] = defaultdict(list)
    for row in filtered:
        by_impl[str(row["subject_key"])].append(row)
    for impl_key, impl_rows in sorted(by_impl.items()):
        values = [float(row["speedup_pct"]) for row in impl_rows]
        regressions = sorted(
            (row for row in impl_rows if float(row["speedup_pct"]) < -noise),
            key=lambda row: float(row["speedup_pct"]),
        )
        ratios = [1.0 + value / 100.0 for value in values]
        geomean = 100.0 * (math.exp(statistics.fmean(map(math.log, ratios))) - 1.0)
        print(f"[{impl_key}] {len(impl_rows)} points  geomean {geomean:+.2f}%  "
              f"regressions {len(regressions)} (> {noise:g}% slower)")
        for row in regressions[:5]:
            print(f"    {row['datatype']:<28} {int(row['size_bytes']):>9} B  "
                  f"{float(row['speedup_pct']):+7.2f}%  "
                  f"[{float(row['ci_low_pct']):+.2f}, {float(row['ci_high_pct']):+.2f}]")


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
