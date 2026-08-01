# HWPC_CXI Hook Module

This directory contains the Open MPI `hook:hwpc_cxi` component for collecting
and reporting HPE Cassini (CXI) hardware performance counters (HWPC).

For information on what this feature is, what it's for, and how it's used, please see the
Open MPI user documentation in `docs/features/hwpc-cxi.rst` (or at
[docs.open-mpi.org](https://docs.open-mpi.org)).

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
