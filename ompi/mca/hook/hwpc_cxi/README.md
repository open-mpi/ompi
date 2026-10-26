# HWPC_CXI Hook Module

This directory contains the Open MPI `hook:hwpc_cxi` component for collecting
and reporting HPE Cassini (CXI) hardware performance counters (HWPC).

For information on what this feature is, what it's for, and how it's used, please see the
Open MPI user documentation in `docs/features/hwpc-cxi.rst` (or at
[docs.open-mpi.org](https://docs.open-mpi.org)).

## Build And Component Registration

This module is built by default as a static MCA component. Counter collection
is only active when configure finds libcxi (via pkg-config, the default search
paths, or `/usr/local`); otherwise the component is built as a no-op. See the
configure summary's "HWPC-CXI support" line.

Require libcxi (configure aborts if it is not found):

```bash
./configure --enable-hwpc-cxi
./configure --with-libcxi=/path/to/libcxi/prefix
```

Do not build the component:

```bash
./configure --disable-hwpc-cxi
```

Do not build the component:

```bash
./configure --disable-hwpc-cxi
```

Component identity:

- Framework: `hook`
- Component: `hwpc_cxi`

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

Hardware validation assets are under `test-multi/`; hardware-independent
unit tests are under `test/` and run with `make check`.

Quick start:

```bash
cd ompi/mca/hook/hwpc_cxi/test-multi
make hwpc_cxi_sendrecv_test
./run_hwpc_cxi_validate.sh
```
