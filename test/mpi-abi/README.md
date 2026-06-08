<!--
  Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.

  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# MPI ABI tests

This directory contains the MPI standard ABI test runner and generated
test templates.

The normal `make check` path runs fast metadata and manifest checks that
do not require an installed Open MPI, `mpicc_abi`, or `mpirun`.

The installed ABI path is:

```sh
make check-abi
```

`make check-abi` expects the installed Open MPI tools to be available on
`PATH`, unless overridden with environment or make variables.

The optional cross-implementation path is:

```sh
make check-abi-cross
```

The initial cross-implementation target is MPICH.

Useful variables:

* `OMPI_ABI_TEST_MPICC_ABI`: Open MPI standard ABI C wrapper.
* `OMPI_ABI_TEST_MPIRUN`: Open MPI launcher.
* `MPICH_ABI_TEST_MPICC`: MPICH C wrapper.
* `MPICH_ABI_TEST_MPIRUN`: MPICH launcher.
* `OMPI_ABI_TEST_NP1`: one-rank test size.
* `OMPI_ABI_TEST_NP2`: two-rank test size.
* `OMPI_ABI_TEST_TMPDIR`: temporary directory root.
* `OMPI_ABI_TEST_KEEP`: preserve generated files and logs.

The runner emits JSON and text reports under `.mpi-abi/` in the build
tree.  Generated sources, executables, logs, and reports are removed by
the local clean targets unless preservation is requested for a runner
invocation.
