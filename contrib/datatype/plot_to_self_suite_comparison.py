#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

"""Plot a five-scenario to_self comparison as one figure per datatype."""

from __future__ import annotations

import argparse
import csv
import math
import os
import textwrap
from collections import defaultdict
from pathlib import Path


COLORS = ("#1f77b4", "#d95f02", "#2ca02c", "#9467bd")
MARKERS = ("o", "s", "^", "D")
SCENARIO_LABELS = {
    "pack": "MPI_Pack",
    "unpack": "MPI_Unpack",
    "isend_recv_ddt_to_packed": "Isend/Recv: DDT to packed",
    "isend_recv_packed_to_ddt": "Isend/Recv: packed to DDT",
    "isend_recv_ddt_to_ddt": "Isend/Recv: DDT to DDT",
}


def parse_args() -> argparse.Namespace:
    """Parse the merged summary and graph controls."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--noise", type=float, default=3.0)
    parser.add_argument("--statistic", choices=("filtered", "unfiltered"), default="filtered")
    args = parser.parse_args()
    if not math.isfinite(args.noise) or args.noise < 0.0:
        parser.error("--noise must be finite and nonnegative")
    return args


def byte_label(value: float, _position=None) -> str:
    """Format power-of-two payload sizes compactly."""
    if value >= 1024 * 1024 and 0 == int(value) % (1024 * 1024):
        return f"{int(value) // (1024 * 1024)}M"
    if value >= 1024 and 0 == int(value) % 1024:
        return f"{int(value) // 1024}K"
    return str(int(value))


def load_data(path: Path, statistic: str):
    """Group comparison points by datatype, scenario, and implementation."""
    grouped = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    datatype_indexes: dict[str, int] = {}
    baseline_labels: set[str] = set()
    comparison_labels: dict[str, str] = {}
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream, delimiter="\t")
        required = {
            "datatype_index", "datatype", "operation", "size_bytes", "statistic",
            "comparison_key", "comparison_label", "baseline_label", "speedup_pct",
            "ci_low_pct", "ci_high_pct",
        }
        if reader.fieldnames is None or not required.issubset(reader.fieldnames):
            raise RuntimeError(f"{path} does not contain the expected suite columns")
        for row in reader:
            if statistic != row["statistic"]:
                continue
            datatype = row["datatype"]
            scenario = row["operation"]
            comparison = row["comparison_key"]
            datatype_indexes[datatype] = int(row["datatype_index"])
            baseline_labels.add(row["baseline_label"])
            comparison_labels[comparison] = row["comparison_label"]
            grouped[datatype][scenario][comparison].append(
                (
                    int(row["size_bytes"]), float(row["speedup_pct"]),
                    float(row["ci_low_pct"]), float(row["ci_high_pct"]),
                )
            )
    if not grouped:
        raise RuntimeError(f"{path} contains no {statistic} measurements")
    if 1 != len(baseline_labels):
        raise RuntimeError(f"{path} does not use one common baseline")
    return grouped, datatype_indexes, comparison_labels, baseline_labels.pop()


def scenario_order(scenarios) -> list[str]:
    """Keep the requested five scenarios in a stable visual order."""
    known = [scenario for scenario in SCENARIO_LABELS if scenario in scenarios]
    return known + sorted(set(scenarios) - set(known))


def draw_axis(axis, scenario: str, comparisons, styles, noise: float) -> None:
    """Draw one scenario with the common MPICH-relative sign convention."""
    from matplotlib.ticker import FuncFormatter, LogLocator

    axis.axhspan(-noise, noise, color="#d9d9d9", alpha=0.45, linewidth=0)
    axis.axhline(0.0, color="#333333", linewidth=0.8)
    for key, points in sorted(comparisons.items()):
        color, marker, label = styles[key]
        ordered = sorted(points)
        sizes = [point[0] for point in ordered]
        axis.fill_between(
            sizes, [point[2] for point in ordered], [point[3] for point in ordered],
            color=color, alpha=0.12, linewidth=0,
        )
        axis.plot(
            sizes, [point[1] for point in ordered], color=color, marker=marker,
            markersize=3.2, linewidth=1.4, label=label,
        )
    axis.set_xscale("log", base=2)
    axis.xaxis.set_major_locator(LogLocator(base=2, numticks=7))
    axis.xaxis.set_major_formatter(FuncFormatter(byte_label))
    axis.grid(axis="both", color="#bdbdbd", alpha=0.35, linewidth=0.6)
    axis.set_title(SCENARIO_LABELS.get(scenario, scenario.replace("_", " ")), fontsize=9)
    axis.tick_params(axis="both", labelsize=7)


def save_graphs(input_path: Path, output: Path, noise: float, statistic: str) -> None:
    """Create PNG and PDF five-panel figures for every datatype."""
    os.environ.setdefault("MPLCONFIGDIR", str(output / ".matplotlib"))
    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError as error:
        raise RuntimeError("matplotlib is required to generate graphs") from error

    grouped, indexes, labels, baseline = load_data(input_path, statistic)
    comparison_keys = sorted(labels)
    styles = {
        key: (COLORS[index % len(COLORS)], MARKERS[index % len(MARKERS)], labels[key])
        for index, key in enumerate(comparison_keys)
    }
    graph_dir = output / "by_datatype"
    graph_dir.mkdir(parents=True, exist_ok=True)
    for datatype in sorted(grouped, key=lambda name: indexes[name]):
        scenarios = scenario_order(grouped[datatype])
        columns = 2
        rows = math.ceil(len(scenarios) / columns)
        figure, axes = plt.subplots(
            rows, columns, figsize=(12.0, 3.7 * rows), squeeze=False
        )
        figure.subplots_adjust(
            top=0.88, bottom=0.07, left=0.08, right=0.98, hspace=0.35, wspace=0.16
        )
        for position, scenario in enumerate(scenarios):
            row, column = divmod(position, columns)
            draw_axis(axes[row][column], scenario, grouped[datatype][scenario], styles, noise)
        for position in range(len(scenarios), rows * columns):
            row, column = divmod(position, columns)
            axes[row][column].set_visible(False)
        handles, legend_labels = axes[0][0].get_legend_handles_labels()
        figure.legend(
            handles, legend_labels, loc="upper center", bbox_to_anchor=(0.5, 0.99),
            ncol=len(handles), fontsize=9,
        )
        figure.supxlabel("Payload bytes (log2 scale)", fontsize=10)
        figure.supylabel(f"Speedup relative to {baseline} (%)", fontsize=10)
        figure.suptitle(
            f"{indexes[datatype]}: {textwrap.fill(datatype.replace('_', ' '), 72)}\n"
            f"Positive values mean faster than the baseline; shaded band is +/-{noise:g}%",
            fontsize=12, y=0.945,
        )
        stem = f"{indexes[datatype]:02d}_{datatype}"
        figure.savefig(graph_dir / f"{stem}.png", dpi=180, facecolor="white")
        figure.savefig(graph_dir / f"{stem}.pdf", facecolor="white")
        plt.close(figure)


def main() -> int:
    """Load a merged suite table and write per-datatype graphs."""
    args = parse_args()
    output = args.output.expanduser().resolve()
    output.mkdir(parents=True, exist_ok=True)
    save_graphs(args.input.expanduser().resolve(), output, args.noise, args.statistic)
    print(f"Graphs: {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}")
        raise SystemExit(1)
