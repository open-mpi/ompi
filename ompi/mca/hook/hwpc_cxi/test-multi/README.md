<!--
  SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
  SPDX-License-Identifier: BSD-3-Clause-Open-MPI

  Copyright (c) 2026      Copyright Hewlett Packard Enterprise Development LP. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# HWPC_CXI Validation

This directory uses a single supported validation workload and driver:

- `hwpc_cxi_sendrecv_test` - minimal MPI send/recv traffic generator
- `run_hwpc_cxi_validate.sh` - primary validation script

Older multi-test programs in this folder were placeholders and are no longer
part of the maintained workflow.

## Recommended Workflow

1. Build Open MPI with HWPC_CXI enabled.
2. Build `ompi/mca/hook/hwpc_cxi/test-multi`.
3. Set runtime library paths.
4. Run `run_hwpc_cxi_validate.sh`.

## Build

```bash
cd /path/to/hpc-openmpi/ompi/mca/hook/hwpc_cxi/test-multi
make hwpc_cxi_sendrecv_test
./build_with_env_and_run.sh
```

This builds `hwpc_cxi_sendrecv_test` and prints the expected runtime
environment values.

## Runtime Environment

Set the runtime library path placeholder used by the scripts:

```bash
export HWPC_CXI_RUNTIME_LD_LIBRARY_PATH="/path/to/libfabric/lib:/path/to/json-c/lib64"
```

If this variable is left as `__REPLACE_ME_RUNTIME_LD_LIBRARY_PATH__`, the
scripts will fail fast and print a clear error.

## Run Validation

```bash
cd /path/to/hpc-openmpi/ompi/mca/hook/hwpc_cxi/test-multi
./run_hwpc_cxi_validate.sh [num_procs] [num_ppn] [loops]
```

Example:

```bash
./run_hwpc_cxi_validate.sh 4 2 100
```

Optional baseline workflow:

```bash
./run_hwpc_cxi_validate.sh --save-baseline 4 2 100
./run_hwpc_cxi_validate.sh 4 2 100
```

## What Gets Validated

`run_hwpc_cxi_validate.sh` drives `hwpc_cxi_sendrecv_test` and validates:

- stdout and stderr capture
- comparison against baseline logs (when present)
- fake counter-name rejection diagnostics
- report-level behavior (`0`, `1`, and `5`)
- report-file generation checks

## Regression Coverage

The hardware-independent `../test` suite exercises the complete singleton hook
lifecycle with fixture counter files, repeated initialization/finalization,
allocation cleanup, sampling validity, MPI_T callbacks, and report parsing.
`check_hwpc_cxi_driver.sh` runs the real validation driver with a mock `srun`.
It checks launch status propagation, timeout status, missing/malformed reports,
cross-node placement verification, changed hostnames, and preservation of every
baseline file after validation failure. These shell regressions need no MPI or
CXI installation:

```bash
bash ../test/check_hwpc_cxi_compare.sh
bash ../test/check_hwpc_cxi_driver.sh
```

On a freshly configured HWPC-CXI build, build and run the unit tests before
installing. Do not reuse Makefiles copied from another system. CI exercises
both HWPC-CXI and HWPC-CXI with MCA DSOs, treats missing declarations/returns as
errors, and compiles the manual workload without launching it on CI runners.

```bash
make -C ../test check VERBOSE=1
make hwpc_cxi_sendrecv_test
```

The traffic workload discovers a peer outside rank zero's shared-memory domain.
The driver requires cross-node placement by default when `num_procs > num_ppn`.
Set `HWPC_CXI_REQUIRE_CROSS_NODE=1` to require it explicitly; a single-node
allocation then fails instead of silently validating local traffic. Peer
placement proves inter-node communication, not the selected transport. For an
OFI/CXI validation run, explicitly select the intended stack:

```bash
export OMPI_MCA_pml=cm
export OMPI_MCA_mtl=ofi
export FI_PROVIDER=cxi
```

Each run gets a unique directory and default report prefix. Override the log
root with `HWPC_CXI_LOGS_DIR`; every `srun` has the time limit specified by
`HWPC_CXI_SCENARIO_TIME_LIMIT` (default `00:05:00`). Hostname identity is strict
in standalone comparator tests, but normalized for cross-allocation baselines.

## Multi-Rank Fault Injection

Build the existing fixture binary and opt into its multi-rank scenarios:

```bash
make -C ../test hwpc_cxi_counters_test hwpc_cxi_mpi_t_pvars_enabled_test
export HWPC_CXI_FIXTURE_TEST_BIN="$(cd ../test && pwd)/hwpc_cxi_counters_test"
export HWPC_CXI_MPI_T_TEST_BIN="$(cd ../test && pwd)/hwpc_cxi_mpi_t_pvars_enabled_test"
./run_hwpc_cxi_validate.sh 4 2 100
```

The fixture scenarios inject one failure on the last rank during `job`,
`topology`, `comm`, or `collection` setup. Every rank must disable collection,
release resources, and finish before the launch timeout. `success` requires
initialized counters on every leader and a baseline sample on every rank.
The schema cases check equal schemas, equal-count/different-name schemas,
ordering differences, and identical requested lists with different initialized
counts/order. Schema fixtures treat each rank as a logical leader, so they can
also be launched on one node:

```bash
srun --mpi=pmix --time=00:01:00 -n 2 "$HWPC_CXI_FIXTURE_TEST_BIN" --mpi-case schema_actual_count
```

The initialized-only schema cases intentionally require rejection even when the
requested names agree. The current hook checks requested names, so those cases
are expected to expose that outstanding implementation defect. The asymmetric
failure cases also probe early returns and NULL job-data handling; crashes or
timeouts are failures, not expected successes. This test patch does not change
the production initialization or schema-agreement implementation.

The optional MPI_T hardware test validates metadata and handle reads in addition
to the existing post-finalization invalidation check. Both optional groups print
an explicit `SKIP` when their binary paths are not provided.

## Key Files

- `hwpc_cxi_sendrecv_test.c` - minimal workload source
- `run_hwpc_cxi_validate.sh` - validation driver
- `my_desired_cxi_counters.txt` - tracked counters list
- `my_desired_fake_cxi_counters.txt` - invalid counter-name test list
- `HWPC_CXI_GUIDE.md` - detailed step-by-step guide

## Troubleshooting

- Build issues: verify Open MPI was configured with `--enable-hwpc-cxi`.
- Runtime loader issues: verify `HWPC_CXI_RUNTIME_LD_LIBRARY_PATH` contains
  required runtime library directories.
- Missing reports: inspect the run directory under `validation_logs/` for
  stdout/stderr and report index files.
