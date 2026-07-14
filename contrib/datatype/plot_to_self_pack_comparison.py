#!/usr/bin/env python3
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

"""Plot arbitrary named pack or unpack comparisons emitted by the to_self runner."""

from __future__ import annotations

import argparse
import csv
import math
import os
import textwrap
from collections import defaultdict
from pathlib import Path


COLORS = ("#1f77b4", "#d95f02", "#2ca02c", "#9467bd", "#8c564b", "#17becf")
MARKERS = ("o", "s", "^", "D", "v", "P", "X")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="summary.tsv from the runner")
    parser.add_argument("--output", type=Path, required=True, help="graph output directory")
    parser.add_argument("--columns", type=int, default=4, help="columns in the combined figure")
    parser.add_argument("--noise", type=float, default=3.0, help="shaded +/- noise interval")
    parser.add_argument(
        "--exclude-hand-made", action="store_true",
        help="omit the base implementation versus hand-made comparison",
    )
    parser.add_argument(
        "--regression-only", action="store_true",
        help="draw the median line only where the subject regresses (speedup < -noise); "
             "the confidence band is still shown everywhere",
    )
    args = parser.parse_args()
    if args.columns < 1:
        parser.error("--columns must be positive")
    if args.noise < 0.0:
        parser.error("--noise cannot be negative")
    return args


def load_summary(
    path: Path, exclude_hand_made: bool = False,
) -> tuple[
    str,
    dict[str, list[tuple[int, str, list[tuple[str, str, list[tuple[int, float, float, float]]]]]]],
]:
    """Group paired comparison series by statistic and datatype."""
    grouped: dict[
        str, dict[
            tuple[int, str], dict[str, tuple[str, list[tuple[int, float, float, float]]]]
        ],
    ] = defaultdict(lambda: defaultdict(dict))
    operations: set[str] = set()
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream, delimiter="\t")
        required = {
            "datatype_index", "datatype", "size_bytes", "comparison_key", "comparison_label",
            "speedup_pct",
        }
        if reader.fieldnames is None or not required.issubset(reader.fieldnames):
            raise RuntimeError(f"{path} does not contain the expected summary columns")
        for row in reader:
            operations.add(row.get("operation", "pack"))
            statistic = row.get("statistic", "filtered")
            key = (int(row["datatype_index"]), row["datatype"])
            comparison_key = row["comparison_key"]
            if exclude_hand_made and "base_vs_hand" == comparison_key:
                continue
            comparison_label = row["comparison_label"]
            comparisons = grouped[statistic][key]
            if comparison_key not in comparisons:
                comparisons[comparison_key] = (comparison_label, [])
            elif comparisons[comparison_key][0] != comparison_label:
                raise RuntimeError(f"{path} changes the label for {comparison_key}")
            speedup = float(row["speedup_pct"])
            comparisons[comparison_key][1].append(
                (
                    int(row["size_bytes"]), speedup,
                    float(row.get("ci_low_pct", speedup)),
                    float(row.get("ci_high_pct", speedup)),
                )
            )
    if not grouped:
        raise RuntimeError(f"{path} contains no measurements")
    if 1 != len(operations):
        raise RuntimeError(f"{path} mixes operations")
    return operations.pop(), {
        statistic: [
            (
                index, datatype,
                [
                    (comparison_key, label, sorted(points))
                    for comparison_key, (label, points) in comparisons.items()
                ],
            )
            for (index, datatype), comparisons in sorted(datatypes.items())
        ]
        for statistic, datatypes in grouped.items()
    }


def byte_label(value: float, _position=None) -> str:
    """Format powers of two compactly without hiding the actual packed byte scale."""
    if value >= 1024 * 1024 and 0 == int(value) % (1024 * 1024):
        return f"{int(value) // (1024 * 1024)}M"
    if value >= 1024 and 0 == int(value) % 1024:
        return f"{int(value) // 1024}K"
    return str(int(value))


def comparison_styles(datatypes) -> dict[str, tuple[str, str]]:
    """Assign every comparison a stable color and marker across all subplots.

    Comparison keys are collected from every datatype, not just the first one:
    a later datatype may carry a comparison the first one lacks (e.g. a backend
    that only ran for some datatypes), and drawing it would otherwise raise a
    KeyError. First-appearance ordering keeps colors/markers stable per key.
    """
    keys: list[str] = []
    seen: set[str] = set()
    for _, _, series in datatypes:
        for key, _, _ in series:
            if key not in seen:
                seen.add(key)
                keys.append(key)
    return {
        key: (COLORS[position % len(COLORS)], MARKERS[position % len(MARKERS)])
        for position, key in enumerate(keys)
    }


def draw_datatype(
    axis, index: int, datatype: str, series, styles: dict[str, tuple[str, str]], noise: float,
    show_labels: bool, regression_only: bool = False,
) -> None:
    """Draw every named comparison with a common sign convention and noise band.

    In regression_only mode the confidence band is still drawn for every point,
    but the median line and its markers are shown only where the subject is
    slower than the baseline beyond the noise band (speedup < -noise): the plot
    then highlights regressions and stays quiet everywhere else.
    """
    from matplotlib.ticker import FuncFormatter, LogLocator

    axis.axhspan(-noise, noise, color="#d9d9d9", alpha=0.45, linewidth=0)
    axis.axhline(0.0, color="#333333", linewidth=0.8)
    for comparison_key, comparison_label, points in series:
        color, marker = styles[comparison_key]
        sizes = [point[0] for point in points]
        axis.fill_between(
            sizes, [point[2] for point in points], [point[3] for point in points],
            color=color, alpha=0.12, linewidth=0,
        )
        line_values = [point[1] for point in points]
        if regression_only:
            # Keep only regression points; break the line elsewhere with NaN so
            # non-regression points contribute no line segment and no marker.
            line_values = [value if value < -noise else float("nan") for value in line_values]
        axis.plot(
            sizes, line_values, color=color,
            marker=marker, markersize=3.2, linewidth=1.4, label=comparison_label,
        )
    axis.set_xscale("log", base=2)
    axis.xaxis.set_major_locator(LogLocator(base=2, numticks=7))
    axis.xaxis.set_major_formatter(FuncFormatter(byte_label))
    axis.grid(axis="both", color="#bdbdbd", alpha=0.35, linewidth=0.6)
    axis.set_title(f"{index}: {textwrap.fill(datatype.replace('_', ' '), 28)}", fontsize=9)
    axis.tick_params(axis="both", labelsize=7)
    if show_labels:
        axis.set_xlabel("Packed bytes", fontsize=8)
        axis.set_ylabel("Speedup (%)", fontsize=8)


def save_individual_graphs(
    plt, datatypes, styles, output: Path, noise: float, statistic: str, operation: str,
    regression_only: bool = False,
) -> None:
    """Keep one readable graph per datatype in addition to the all-datatype figure."""
    statistic_label = "Tukey-filtered" if "filtered" == statistic else statistic
    operation_label = operation.replace("_", " ")
    convention = (
        "line drawn only where subject regresses beyond the noise band; band is bootstrap 95% CI\n"
        "negative % = subject slower than baseline (regression)"
        if regression_only else
        "line is paired median, band is bootstrap 95% CI\n"
        "positive % = subject faster than baseline"
    )
    individual = output / "by_datatype"
    individual.mkdir(parents=True, exist_ok=True)
    for index, datatype, series in datatypes:
        figure, axis = plt.subplots(figsize=(7.2, 4.2), constrained_layout=True)
        draw_datatype(axis, index, datatype, series, styles, noise, True, regression_only)
        axis.legend(loc="best", fontsize=8)
        figure.suptitle(
            f"{operation_label.capitalize()} ({statistic_label} trials); {convention}",
            fontsize=10,
        )
        figure.savefig(individual / f"{index:02d}_{datatype}.png", dpi=180, facecolor="white")
        plt.close(figure)


def save_combined_graph(
    plt, datatypes, styles, output: Path, columns: int, noise: float, statistic: str,
    operation: str, regression_only: bool = False,
) -> None:
    """Place every datatype graph in one large, reviewable subplot matrix."""
    statistic_label = "Tukey-filtered" if "filtered" == statistic else statistic
    operation_label = operation.replace("_", " ")
    columns = min(columns, len(datatypes))
    rows = math.ceil(len(datatypes) / columns)
    figure_height = max(4.2, 3.1 * rows)
    figure, axes = plt.subplots(
        rows, columns, figsize=(max(7.2, 4.6 * columns), figure_height), squeeze=False
    )
    figure.subplots_adjust(
        top=1.0 - 1.25 / figure_height, bottom=0.55 / figure_height,
        left=0.09, right=0.98, wspace=0.28, hspace=0.35,
    )
    for position, (index, datatype, series) in enumerate(datatypes):
        row, column = divmod(position, columns)
        draw_datatype(axes[row][column], index, datatype, series, styles, noise, False,
                      regression_only)
    for position in range(len(datatypes), rows * columns):
        row, column = divmod(position, columns)
        axes[row][column].set_visible(False)

    # A later datatype may carry a comparison series the first subplot lacks, so
    # collect labels from every axis (dedup by label) rather than just axes[0][0].
    handles_by_label: dict[str, object] = {}
    for axis_row in axes:
        for axis in axis_row:
            for handle, label in zip(*axis.get_legend_handles_labels()):
                handles_by_label.setdefault(label, handle)
    handles = list(handles_by_label.values())
    labels = list(handles_by_label.keys())
    figure.legend(
        handles, labels, loc="upper center", ncol=max(1, min(4, len(labels))), fontsize=9,
        bbox_to_anchor=(0.5, 1.0 - 0.55 / figure_height),
    )
    convention = (
        "regressions only: line drawn where subject is slower beyond the noise band, "
        "band is bootstrap 95% CI\nnegative % = subject slower than baseline (regression)"
        if regression_only else
        "paired median and bootstrap 95% CI\n"
        "positive % = subject faster than baseline"
    )
    figure.suptitle(
        f"{operation_label.capitalize()} performance ({statistic_label} trials): {convention}",
        fontsize=12, y=1.0 - 0.12 / figure_height,
    )
    figure.supxlabel("Packed bytes (log2 scale)", fontsize=11)
    figure.supylabel("Named comparison speedup (%)", fontsize=11)
    figure.savefig(
        output / f"all_datatypes_{operation}_comparison.pdf", bbox_inches="tight", facecolor="white"
    )
    figure.savefig(
        output / f"all_datatypes_{operation}_comparison.png", dpi=180, bbox_inches="tight",
        facecolor="white",
    )
    plt.close(figure)


def main() -> int:
    args = parse_args()
    output = args.output.expanduser().resolve()
    output.mkdir(parents=True, exist_ok=True)
    os.environ.setdefault("MPLCONFIGDIR", str(output / ".matplotlib"))
    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError as error:
        raise RuntimeError("matplotlib is required to generate graphs") from error

    operation, grouped = load_summary(args.input.expanduser().resolve(), args.exclude_hand_made)
    for statistic, datatypes in grouped.items():
        statistic_output = output if "filtered" == statistic else output / statistic
        statistic_output.mkdir(parents=True, exist_ok=True)
        styles = comparison_styles(datatypes)
        save_individual_graphs(
            plt, datatypes, styles, statistic_output, args.noise, statistic, operation,
            args.regression_only,
        )
        save_combined_graph(
            plt, datatypes, styles, statistic_output, args.columns, args.noise, statistic, operation,
            args.regression_only,
        )
    print(f"Graphs: {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}")
        raise SystemExit(1)
