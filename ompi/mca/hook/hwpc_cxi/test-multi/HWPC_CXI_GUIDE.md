<!--
  SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
  SPDX-License-Identifier: BSD-3-Clause-Open-MPI

  Copyright (c) 2026      Copyright Hewlett Packard Enterprise Development LP. All rights reserved.
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
2. Build `hwpc_cxi_sendrecv_test` in `test-multi` after configuring and building Open MPI.
3. Set runtime library paths.
4. Run `run_hwpc_cxi_validate.sh`.

## Build Open MPI

Configure Open MPI with `--enable-hwpc-cxi` and complete the main build before
building this test. The source and build roots are the same for an in-tree
build; for a VPATH build, they are different directories.

## Build The Tests

### Recommended

```bash
export HWPC_CXI_BUILD_ROOT=/path/to/configured/open-mpi-build
cd /path/to/open-mpi-source/ompi/mca/hook/hwpc_cxi/test-multi
./build_with_env_and_run.sh
```

### Manual Configured Build

```bash
export HWPC_CXI_BUILD_ROOT=/path/to/configured/open-mpi-build
make -C "$HWPC_CXI_BUILD_ROOT/ompi/mca/hook/hwpc_cxi/test-multi" hwpc_cxi_sendrecv_test
```

For an in-tree build, set `HWPC_CXI_BUILD_ROOT` to the Open MPI source root.
For a VPATH build, set it to the directory from which Open MPI was configured.

### Build Helper

`build_with_env_and_run.sh` is the preferred path for the standard configured
build environment.

## Runtime Environment

The validation script requires a runtime library path value through:

- `HWPC_CXI_RUNTIME_LD_LIBRARY_PATH`

Example:

```bash
export HWPC_CXI_RUNTIME_LD_LIBRARY_PATH="/path/to/libfabric/lib:/path/to/open-mpi/lib"
```

The scripts prepend this value to `LD_LIBRARY_PATH` at runtime.

The helper uses `LIBFABRIC_PREFIX` to prepend its `lib` directory to runtime
library paths, and `OMPI_PREFIX` to prepend its `bin` and `lib` directories to
executable and runtime library paths. Neither variable selects dependencies for
the configured Automake build. Configure dependency locations, compiler flags
(`CPPFLAGS`), and linker flags (`LDFLAGS`) when configuring Open MPI.

## Run The Tests

### Run The Maintained Validation Flow

```bash
cd "$PROJECT_ROOT/ompi/mca/hook/hwpc_cxi/test-multi"
./run_hwpc_cxi_validate.sh 4 2 100
```

To save a baseline for future comparisons:

```bash
./run_hwpc_cxi_validate.sh --save-baseline 4 2 100
```

### Multi-Node Examples

```bash
srun -N 2 -n 8 --ntasks-per-node=4 ompi/mca/hook/hwpc_cxi/test-multi/hwpc_cxi_sendrecv_test 100
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
- Validation script cannot find executable: configure Open MPI and manually build `hwpc_cxi_sendrecv_test` in `test-multi`.

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
