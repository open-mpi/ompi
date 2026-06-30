/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier:  MIT
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

# HWPC_CXI Validation

This directory uses a single supported validation workload and driver:

- `hwpc_cxi_sendrecv_test` - minimal MPI send/recv traffic generator
- `run_hwpc_cxi_validate.sh` - primary validation script

Older multi-test programs in this folder were placeholders and are no longer
part of the maintained workflow.

## Recommended Workflow

1. Build Open MPI with HWPC_CXI enabled.
2. Build `test/hwpc_cxi`.
3. Set runtime library paths.
4. Run `run_hwpc_cxi_validate.sh`.

## Build

```bash
cd /path/to/hpc-openmpi/test/hwpc_cxi
./build_and_run.sh
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
cd /path/to/hpc-openmpi/test/hwpc_cxi
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
