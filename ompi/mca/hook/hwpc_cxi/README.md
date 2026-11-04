# HWPC_CXI Hook Module

This directory contains the Open MPI `hook:hwpc_cxi` component for collecting
and reporting HPE Cassini (CXI) hardware performance counters (HWPC).

For information on what this feature is, what it's for, and how it's used, please see the
Open MPI user documentation in `docs/features/hwpc-cxi.rst` (or at
[docs.open-mpi.org](https://docs.open-mpi.org)).

## Build And Component Registration

This module is built by default as a static MCA component and has no build-time
dependencies. When counter collection is enabled, `MPI_Init()` aborts the job
if a node lacks a Cassini device (no `/sys/class/cxi/cxi<N>` entry).

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
  hook entry points and MCA variables, all hidden from libmpi's exports.
- `hook_hwpc_cxi_counters.c`:
  core collection, reduction, and reporting implementation.
- `hook_hwpc_cxi_constants.h` and `hook_hwpc_cxi_constants.c`:
  predefined counter groups/mnemonics and lookup utilities.

## Validation

Unit tests are under `test/` and run with `make check`. The tests that call
`MPI_Init()` with collection enabled need a Cassini device and skip (exit 77)
without one; the other tests need no device.

### Multi-rank scenarios (manual)

`hwpc_cxi_counters_test` also has multi-rank scenarios that check that
initialization agreement and counter-schema agreement never hang and disable
collection on every rank when one rank fails. They need a launcher, so
`make check` does not run them. Counter files are simulated, so no Cassini
device is needed.

Build the test with `make check` (or `make hwpc_cxi_counters_test`) in
`ompi/mca/hook/hwpc_cxi/test`, then run each scenario from that directory:

```bash
mpirun -np 2 ./hwpc_cxi_counters_test --mpi-case <name>
```

Each scenario prints `PASS: MPI fixture <name>` from rank 0 and exits 0 on
success. With fewer than 2 ranks it exits 77 (skipped).

| Scenario | What it checks | Minimum to be meaningful |
| --- | --- | --- |
| `success` | Every rank completes initialization and takes a baseline sample | 2 ranks; 2 nodes (`-np 4 --map-by ppr:2:node`) to exercise more than one node leader |
| `job` | Job-data allocation fails on the last rank | 2 ranks |
| `topology` | Hostname allocation fails on the last rank | 2 ranks |
| `comm` | The node-leader communicator split fails on the last rank | 2 ranks |
| `collection` | Counter-collection allocation fails on the last rank | 2 ranks |
| `schema_equal` | Identical counter lists are accepted | 2 ranks |
| `schema_names` | A different requested counter name is rejected | 2 ranks |
| `schema_order` | A different requested counter order is rejected | 2 ranks |
| `schema_actual_count` | A different number of initialized counters is rejected | 2 ranks |
| `schema_actual_order` | A different order of initialized counters is rejected | 2 ranks |

In the failure scenarios, every rank must end with collection disabled and no
rank may hang. The `schema_*` scenarios treat every rank as a node leader, so
one node is enough.
