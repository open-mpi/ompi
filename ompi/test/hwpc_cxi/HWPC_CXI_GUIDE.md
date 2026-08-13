<!--
  SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
  SPDX-License-Identifier: BSD-3-Clause-Open-MPI

  Copyright (c) 2026      Copyright Hewlett Packard Enterprise Development LP
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# HWPC_CXI Test Suite Guide

This file condenses the HWPC_CXI documentation in this folder into one practical reference for building, running, and troubleshooting the tests.

## What This Suite Covers

The maintained HWPC_CXI validation flow uses:

- `hwpc_cxi_sendrecv_test` - minimal MPI workload for counter activity
- `run_hwpc_cxi_validate.sh` - primary validation and comparison driver. Will have to first use the --save-baseline option to create a known good to compare against

Helper files in this directory:

- `build_with_env_and_run.sh` - recommended build helper

## Recommended Workflow

1. Build Open MPI with HWPC_CXI support.
2. Build `hwpc_cxi_sendrecv_test` in `test/hwpc_cxi`.
3. Set runtime library paths.
4. Run `run_hwpc_cxi_validate.sh`.

## Build Open MPI

If you are building manually, make sure HWPC_CXI is enabled at configure time (--enable-hwpc-cxi) and Open MPI finishes building before compiling the test suite.

## Build The Tests

### Recommended

```bash
cd "$PROJECT_ROOT/test/hwpc_cxi"
./build_with_env_and_run.sh
```

### Manual Build With Environment Variables - Example

```bash
cd "$PROJECT_ROOT/test/hwpc_cxi"

export LIBFABRIC_PREFIX=/path/to/libfabric/install/prefix
export OMPI_PREFIX=/path/to/ompi/install/prefix
export LD_LIBRARY_PATH="$LIBFABRIC_PREFIX/lib:$OMPI_PREFIX/lib:${LD_LIBRARY_PATH:-}"

make clean
LIBFABRIC_PREFIX="$LIBFABRIC_PREFIX" \
OMPI_PREFIX="$OMPI_PREFIX" \
make all
```

### Note On The Two Builder Scripts

- `build_with_env_and_run.sh` is the preferred path for the standard build environment

## Runtime Environment

The validation script requires a runtime library path value through:

- `HWPC_CXI_RUNTIME_LD_LIBRARY_PATH`

Example:

```bash
export HWPC_CXI_RUNTIME_LD_LIBRARY_PATH="/path/to/libfabric/lib:/path/to/open-mpi/lib"
```

The scripts prepend this value to `LD_LIBRARY_PATH` at runtime.

Typical environment variables used by the build system:

| Variable | Purpose |
| --- | --- |
| `LIBFABRIC_PREFIX` | libfabric install prefix |
| `CPPFLAGS` | includes libfabric and OpenMPI headers |
| `LDFLAGS` | libfabric and OpenMPI link and rpath flags |

## Run The Tests

### Run The Maintained Validation Flow

```bash
cd "$PROJECT_ROOT/test/hwpc_cxi"
./run_hwpc_cxi_validate.sh 4 2 100
```

To save a baseline for future comparisons:

```bash
./run_hwpc_cxi_validate.sh --save-baseline 4 2 100
```

### Compatibility Entry Point

```bash
./run_hwpc_cxi_tests.sh 4 2 100
```

`run_hwpc_cxi_tests.sh` now forwards directly to `run_hwpc_cxi_validate.sh`.

### Multi-Node Examples

```bash
srun -N 2 -n 8 --ntasks-per-node=4 test/hwpc_cxi/hwpc_cxi_sendrecv_test 100
./run_hwpc_cxi_validate.sh 8 4 100
```

## Expected Results

Successful validation runs generally show:

- `hwpc_cxi_sendrecv_test` completion in stdout
- no missing/invalid-counter errors in baseline comparisons
- expected stderr diagnostics for fake counters
- report-level checks passing according to configured logic
- report files generated where expected

## Troubleshooting

- Build fails with missing HWPC_CXI support: verify configure used `--enable-hwpc-cxi` and the Open MPI build completed.
- Runtime library not found: set `HWPC_CXI_RUNTIME_LD_LIBRARY_PATH` before running validation.
- Validation script cannot find executable: rebuild from `test/hwpc_cxi` and confirm `hwpc_cxi_sendrecv_test` exists.

## Quick Maintenance Checklist

When changing HWPC_CXI code, use this order:

1. `./build_with_env_and_run.sh`
2. `./run_hwpc_cxi_validate.sh --save-baseline 4 2 100` (when baseline refresh is needed)
3. make your desired code changes
4. `./run_hwpc_cxi_validate.sh 4 2 100`

If you are debugging build or runtime failures, start with `build_with_env_and_run.sh` and `run_hwpc_cxi_validate.sh` because this is the maintained path.

## Minimal Summary

- Build Open MPI
- Build and run tests with `./build_with_env_and_run.sh`
- Run `./run_hwpc_cxi_validate.sh 4 2 100`
