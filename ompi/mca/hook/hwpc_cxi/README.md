# HWPC_CXI Hook Module

This directory contains the Open MPI `hook:hwpc_cxi` component for collecting
and reporting HPE Cassini (CXI) hardware performance counters (HWPC).

The feature determines what to track during `MPI_Init()` and emits reports
during `MPI_Finalize()`. Depending on verbosity settings, reporting can include
global summary statistics to stdout and optional host-specific detail files with
NIC-level counter deltas. Counter data is sampled at init/finalize boundaries and 
does not inject report work into the main application execution interval.

## Build And Component Registration

This module is controlled by Open MPI configure-time enablement and is compiled
as a static MCA component.

Enable during configure:

```bash
./configure --enable-hwpc-cxi
```

Component identity:

- Framework: `hook`
- Component: `hwpc_cxi`

## What The Feature Does

The HWPC_CXI feature supports:

- Tracking default timeout-related counters.
- Tracking additional counter sets at higher report levels.
- Accepting user-selected counters from a file instead of default extended sets.
- Accepting counter group names (case-insensitive), individual mnemonic names,
  and exact counter names (including categorized forms).
- Emitting one-line network timeout summaries, aggregate min/mean/max + rate
  summaries, and optional NIC-level detail reports.

A network timeout is treated as a Slingshot network event (for example, a link
flap) that causes packet reissue from a NIC perspective. One event may surface
as multiple NIC-level timeouts.

Unless higher verbosity/reporting is requested, the one-line timeout summary can
be suppressed when no timeouts are detected.

## Runtime Configuration

Runtime options are configured through MCA parameters.

This feature is intended for Slingshot 11 (or newer) environments.

Two common forms are supported:

Environment variables:

```bash
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_report=2
export OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros=true
```

`mpirun` / `mpiexec` MCA options:

```bash
mpirun --mca hook_hwpc_cxi_counter_report 2 ...
```

## MCA Options

### OMPI_MCA_ompi_hook_hwpc_cxi_counter_report

Controls whether Cassini counters are collected and the verbosity of reporting.

- `0`: no Cassini counters collected; feature disabled
- `1`: network timeout counters collected, one-line display
- `2`: option 1 + CXI counter summary report
- `3`: option 2 + NIC detail for NICs that hit a timeout
- `4`: option 2 + NIC detail for all NICs if any timeout occurred
- `5`: option 2 + NIC detail for all NICs

Operational notes:

- Valid range is `0` to `5`.
- Extra counter summaries are available from level `2` and above.
- NIC-level details are enabled from level `3` and above.
- To obtain meaningful delta reports, runtimes should be at least a few
  seconds.

### OMPI_MCA_ompi_hook_hwpc_cxi_counter_verbose

Enables verbose diagnostics about selected and collected counters.

- Type: boolean
- Default: `false`

### OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros

Controls whether zero-delta counters are filtered from summary.

- Type: boolean
- Default: `true`

### OMPI_MCA_ompi_hook_hwpc_cxi_counter_file

Path to a file with an alternate list of counter names to collect.

- If set, this list is used for extended counter selection.
- Default timeout counters are still collected in addition.
- Retry-handler counters should be prefixed with `rh:`.
- Recommended with `OMPI_MCA_ompi_hwpc_cxi_counter_report >= 2`.
- Default: not set

### OMPI_MCA_ompi_hook_hwpc_cxi_counter_report_file

Optional filename prefix for writing detailed reports to host-specific files.

- Used by report levels `3`, `4`, and `5`.
- Output naming pattern:
  `<hwpc_cxi_counter_report_file>.<hostname>`
- Useful on large jobs where stdout can be mixed or truncated.
- Default: not set

## Output Behavior Summary

At finalize time, output may include:

- A one-line network timeout summary (for non-quiet modes).
- A global CXI summary table with min/mean/max and rates (levels `2`+).
- NIC-level detailed counters in stdout or report files (levels `3`+).

If no report-file prefix is set, module defaults are used for detail output
naming.

## Counter Naming And Inputs

User input can reference:

- Counter groups (case-insensitive), for example `CxiPerfStats`.
- Individual mnemonic names.
- Exact counter names, including category/classification variants.

For detailed Cassini counter semantics and taxonomy, refer to the HPE Cassini
performance guide documentation.

## Key Source Files

- `hook_hwpc_cxi_component.c`:
  MCA component and variable registration.
- `hook_hwpc_cxi.h`:
  exported module declarations and MCA globals.
- `hook_hwpc_cxi_counters.c`:
  core collection, reduction, and reporting implementation.
- `hook_hwpc_cxi_constants.h` and `hook_hwpc_cxi_constants.c`:
  predefined counter groups/mnemonics and lookup utilities.

## Validation

Validation assets are under `test/hwpc_cxi/`.

Quick start:

```bash
cd test/hwpc_cxi
./run_hwpc_cxi_validate.sh
```

## Notes

- Zero filtering applies to summary counter rows; headings and context lines may
  still appear depending on report mode.
- If no `counter_report_file` is provided, the module generates a default
  prefix.
