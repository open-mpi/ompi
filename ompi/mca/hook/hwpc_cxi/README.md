# HWPC_CXI Hook Module

This directory contains the Open MPI `hook:hwpc_cxi` component for collecting
and reporting HPE Cassini (CXI) hardware performance counters (HWPC).

For information on what this feature is, what it's for, and how it's used, please see the
Open MPI user documentation in `docs/features/hwpc-cxi.rst` (or at
[docs.open-mpi.org](https://docs.open-mpi.org)).

## Build And Component Registration

This module is built by default as a static MCA component and has no build-time
dependencies. At `MPI_Init()`, counter collection is disabled for the job if any
node lacks a Cassini device (`/sys/class/cxi/cxi0`).

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

Hardware-independent unit tests are under `test/` and run with `make check`.
