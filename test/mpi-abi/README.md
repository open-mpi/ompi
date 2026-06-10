<!--
  Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.

  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# MPI ABI tests

This directory contains the MPI standard ABI test runner and generated
test templates.

The MPI ABI tests focus on Open MPI's standard ABI layer: the installed
standard ABI header, wrapper compiler, ABI library, ABI helper symbols,
handle/value conversion code, and public `MPI_*` entry points reached
through that ABI path.  They are not intended to replace Open MPI's
general MPI correctness tests.  A major assumption is that the normal
Open MPI implementation below the ABI layer is already tested elsewhere;
these tests check that the ABI layer correctly exposes, translates, and
forwards calls into that implementation.

## Test groups

The suite has several complementary groups of checks:

* Metadata, manifest, and source checks.

  These are fast checks that run from the source and build trees.  They
  compare the ABI metadata under `docs/` with the runner's manifest,
  classification rules, generated-source contracts, C header constants,
  and Fortran helper source contracts.  They are useful because they
  catch drift between the MPI-standard-derived ABI description and what
  the test suite believes is implemented, skipped, or still uncovered.

* Installed header, wrapper, link, and symbol checks.

  These checks use the installed `mpicc_abi` wrapper and installed
  standard ABI header.  They verify that the wrapper advertises the
  standard ABI include and link path, the installed header declares the
  implemented standard ABI C APIs with signatures matching the
  MPI-standard-derived binding metadata, non-ABI C APIs are not
  accidentally declared, aggregate compile/link probes can force
  references to the ABI entry points, and optional symbol-table
  diagnostics see defined `MPI_*` / `PMPI_*` exports in the ABI library.
  This group is effective at catching packaging, installation, header
  generation, signature, and symbol-export regressions before any MPI
  runtime job is launched.

* ABI converter and handle/value checks.

  These installed C probes exercise the standard ABI helper functions
  such as `MPI_Comm_toint`, `MPI_Comm_fromint`, `MPI_Type_toint`, and
  their PMPI equivalents.  They round-trip predefined handles, null
  handles, dynamic handles, status sentinels, error classes, keyval
  sentinels, and configured datatype constants.  These checks target the
  core ABI responsibility: converting between standard ABI integer
  values and Open MPI's internal handle representation without losing
  special values, optional constants, or dynamically-created objects.

* Installed MPI runtime probes.

  These are real MPI programs built with installed `mpicc_abi` and run
  with installed `mpirun`.  Each logical probe is compiled into its own
  executable because MPI process state is undefined after many runtime
  failures.  The probes call public `MPI_*` APIs through the ABI layer
  and validate return codes, output handles, statuses, counts, data
  movement, communicator/group/window/file state, request completion,
  RMA results, MPI-IO results, and other observable side effects.
  There is no mocking of OMPI back-end functions: successful probes have
  traversed the standard ABI entry point, ABI conversion/shim code, and
  the normal Open MPI implementation underneath.

* Callback and retained-lifetime probes.

  Callback-heavy APIs need separate runtime probes because callback
  failures can leave MPI objects, requests, or process state undefined.
  These checks cover attribute copy/delete callbacks, error-handler
  callbacks, user-defined operation callbacks, generalized request
  callbacks, datarep callbacks, MPI_T callbacks where implemented, and
  any operation that retains converted ABI values across nonblocking or
  persistent lifetimes.  They are separated from the ordinary runtime
  probes so the test runner can keep each risky callback scenario
  isolated.

* Fortran binding regression checks.

  Open MPI does not currently implement the MPI-5 Fortran standard ABI
  as an ABI-capable `mpi_f08` module and wrapper path analogous to the
  C `mpicc_abi` path.  In particular, there is no `mpifort_abi`
  wrapper in this implementation.  The Fortran checks in this suite
  therefore do not claim exhaustive MPI Fortran ABI coverage.  They
  compile and run selected `mpif.h`, `use mpi`, and `use mpi_f08`
  programs through Open MPI's normal `mpifort` wrapper to catch
  regressions in the existing Fortran bindings, and they exercise the
  Fortran `MPI_Abi_*` helper interfaces that Open MPI exposes.  Those
  helper probes intentionally report Open MPI's current behavior: a
  normal Fortran build path is not ABI-capable, so
  `MPI_Abi_get_version` can report `-1, -1` while the other helper
  routines remain callable and are validated for their documented
  Open MPI behavior.  Missing MPI-5 Fortran ABI functionality should be
  recorded as explicitly unimplemented or skipped rather than treated as
  covered by these regression checks.

* MPICH compatibility checks.

  When enabled, the MPICH path compares Open MPI's standard ABI behavior
  against MPICH.  This is useful for identifying places where Open MPI
  diverges from the MPI standard ABI contract rather than merely being
  internally self-consistent.

Together, these groups test the ABI layer from several directions:
metadata authority, installed artifacts, symbol reachability, handle
translation, complete public API call paths, callback conversion, and
MPICH compatibility behavior.  Passing these tests does not prove that
every underlying MPI algorithm is correct; it proves that the ABI-facing
surface is consistent with the standard ABI metadata and can successfully
drive the already-tested Open MPI implementation through the ABI layer.

## Running the tests

The normal `make check` path runs fast metadata and manifest checks that
do not require an installed Open MPI, `mpicc_abi`, or `mpirun`.

The installed ABI path is:

```sh
make check-abi
```

`make check-abi` expects the installed Open MPI tools to be available on
`PATH`, unless overridden with environment or make variables.

The optional MPICH compatibility path is:

```sh
make check-abi-mpich
```

`make check-abi-mpich` expects both an installed Open MPI with standard
ABI support and an installed MPICH with MPI Forum ABI support.  MPICH's
normal internal ABI is not the MPI Forum ABI.  For MPICH 5.0.x, this
requires configuring MPICH with `--enable-mpi-abi` so that it installs
the MPI Forum ABI wrapper (`mpicc_abi`), header (`mpi_abi.h`), and ABI
libraries (`libmpi_abi` and `libpmpi_abi`).

By default, put both installations' `bin` directories on `PATH`; the
order does not matter because the runner classifies the discovered tools
before selecting them.  The Open MPI install must provide the standard
ABI C wrapper (`mpicc_abi`) and launcher (`mpirun`); the normal Open MPI
`mpicc` wrapper is not a substitute because it links Open MPI's internal
ABI, not `libmpi_abi`.

MPICH is discovered from a real MPICH-looking MPI Forum ABI wrapper and
launcher.  A generic `mpicc` on `PATH` is not sufficient: it may be Open
MPI's wrapper, and even MPICH's normal `mpicc` links MPICH's internal ABI
rather than the MPI Forum ABI.  If MPICH uses site-specific wrapper
names or is installed outside `PATH`, set `MPICH_ABI_TEST_MPICC` to the
MPICH MPI Forum ABI C wrapper, normally `mpicc_abi`, and set
`MPICH_ABI_TEST_MPIRUN` to the matching launcher.

Explicit tool overrides are treated as operator intent.  If an override
is set, the runner validates that tool instead of silently falling back
to another executable from `PATH`.  A launcher-only override is paired
only with compiler wrappers discovered in the same installation
directory; if the compiler wrapper is outside `PATH`, set the matching
compiler override as well.  Missing or invalid Open MPI or MPICH
prerequisites are reported as failures, not skips, because invoking this
target is an explicit request for MPICH compatibility results.

The MPICH target records both standard ABI directions:

* compile with MPICH and run against Open MPI's standard ABI runtime;
* compile with Open MPI's `mpicc_abi` and run against MPICH.

When the cross-direction runtime probes are enabled, the runner will
sanitize the platform runtime library path for each launched executable.
On Linux this means replacing `LD_LIBRARY_PATH`; on macOS it means
replacing `DYLD_LIBRARY_PATH`.  The replacement contains only the
selected run-side MPI library directory unless an explicit library-path
override is supplied.  This avoids accidentally compiling with one MPI
implementation and then loading a stale `libmpi` from the user's shell
environment at run time.

Until the MPICH probe execution work is complete, `make check-abi-mpich`
reports the discovered environment and marks the per-direction runtime
probes as deferred with a stable skip reason.

Useful variables:

* `OMPI_ABI_TEST_MPICC_ABI`: Open MPI standard ABI C wrapper.
* `OMPI_ABI_TEST_MPIRUN`: Open MPI launcher.
* `OMPI_ABI_TEST_INCLUDE_PATH`: Open MPI standard ABI include override.
* `OMPI_ABI_TEST_LIBRARY_PATH`: Open MPI ABI library path override.
* `OMPI_ABI_TEST_LAUNCHER_ARGS`: extra Open MPI launcher arguments.
* `MPICH_ABI_TEST_MPICC`: MPICH MPI Forum ABI C wrapper, normally
  `mpicc_abi`.
* `MPICH_ABI_TEST_MPIRUN`: MPICH launcher.
* `MPICH_ABI_TEST_INCLUDE_PATH`: MPICH include path override.
* `MPICH_ABI_TEST_LIBRARY_PATH`: MPICH library path override.
* `MPICH_ABI_TEST_LAUNCHER_ARGS`: extra MPICH launcher arguments.
* `OMPI_ABI_TEST_MPICH_DIRECTIONS`: comma-separated MPICH direction
  selection.  Valid values are `both`, `mpich-to-ompi`, and
  `ompi-to-mpich`.  Invalid values are setup failures because they
  would otherwise hide a CI configuration mistake.
* `OMPI_ABI_TEST_DYNAMIC_PROCESS`: set to `0` to skip dynamic process
  probes in launch environments that cannot service spawn, connect,
  accept, or name-service operations.
* `OMPI_ABI_TEST_NP1`: one-rank test size.
* `OMPI_ABI_TEST_NP2`: two-rank test size.
* `OMPI_ABI_TEST_TMPDIR`: temporary directory root.
* `OMPI_ABI_TEST_KEEP`: preserve generated files and logs.

The runner emits JSON and text reports under `.mpi-abi/` in the build
tree.  Generated sources, executables, logs, and reports are removed by
the local clean targets unless preservation is requested for a runner
invocation.
