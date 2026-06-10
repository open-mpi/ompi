<!--
  Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.

  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# MPI ABI test suite specification

## Purpose

Open MPI supports its long-standing internal ABI and, on this branch,
the MPI standard-defined ABI.  The test suite specified here validates
that Open MPI's standard ABI implementation conforms to the MPI-5.x ABI
metadata shipped in this source tree, and that the ABI refactor does not
regress Open MPI's existing C and Fortran MPI bindings.

The suite is intended to be long-lived Open MPI infrastructure.  It
must also be complete enough to validate the ABI work in this branch.

## Authority

The authoritative test inputs are the MPI standard metadata files in
`docs/`:

* `docs/mpi-standard-5.0-apis.json`
* `docs/mpi-standard-5.0-abi.json`
* `docs/mpi-standard-apis.json`
* `docs/mpi-standard-abi.json`

The suite tracks the upstream MPI standard ABI as represented by those
files.  Open MPI may explicitly omit not-yet-implemented ABI
functionality, but it must not silently diverge from MPI
standard-specified ABI behavior.

The test manifest must distinguish omission from divergence:

* omitted, unsupported, or unavailable functionality is reported as a
  machine-readable `SKIP` with a stable reason;
* implemented functionality that differs from MPI standard ABI behavior
  is a hard failure.

## Scope

All MPI APIs represented in the authority metadata are eventually in
scope.

The C MPI API scope is exhaustive standard ABI coverage.  The suite must
eventually cover every standard ABI C entry point, including compile,
link, symbol, conversion, callback, and runtime behavior where
applicable.

The Fortran scope has two parts:

* Exhaustive regression coverage for Open MPI's existing Fortran binding
  layers: `mpif.h`, `use mpi`, and `use mpi_f08`.
* ABI functionality testing for the Fortran ABI functionality Open MPI
  implements.  The MPI standard ABI is understood to define Fortran ABI
  behavior for `use mpi_f08`; Open MPI functionality that is not
  implemented must be explicitly skipped rather than silently ignored.

The suite must include all MPI API families, including but not limited
to initialization, sessions, point-to-point, collectives, persistent
collectives, datatypes, groups, communicators, topology, attributes,
callbacks, error handlers, reduction operators, RMA, MPI-IO, dynamic
process management, and MPI_T.

## Test Classification

The generator must produce a manifest that classifies every API and ABI
constant in the authority metadata.  Each entry must have a stable
machine-readable classification.

* **`implemented`**: Open MPI implements the ABI functionality and tests
  are expected to pass.
* **`not_implemented`**: the standard specifies the ABI functionality,
  but Open MPI does not currently implement it.
* **`not_in_standard_abi`**: the API exists in MPI metadata or Open MPI
  but is not part of the standard ABI surface.
* **`unsupported_by_build`**: the configured Open MPI build does not
  include the feature required to test this API.
* **`unsupported_by_open_mpi`**: Open MPI does not support the
  underlying MPI feature in this branch.
Each entry must also have a stable machine-readable test status.
`test_not_written_yet` means the entry is in scope, but no test case has
been implemented yet.

During phased development of this test suite, `test_not_written_yet` may
be reported without failing the whole suite.  Before this branch is
complete, `test_not_written_yet` must become a hard failure for all APIs
that are in scope for implemented Open MPI ABI functionality.

New MPI metadata entries must fail coverage checking until they are
classified.

## Test Tiers

### make check

Fast in-tree ABI tests should integrate with normal `make check` where
reasonable.  These tests must not require an installed Open MPI, the
`mpicc_abi` wrapper, or `mpirun`.

Examples:

* manifest generation sanity checks;
* ABI metadata validation;
* generated header and constant semantic checks that can run in-tree;
* converter unit tests that do not require launching MPI jobs;
* Fortran ABI helper tests that can run in-tree, without an installed
  wrapper or launcher, when the relevant Fortran bindings are
  configured.

If Open MPI is configured with standard ABI support disabled, these
tests must remain present but skip with a clear machine-readable reason.

### make check-abi

`make check-abi` is the primary installed ABI functional test target.
It requires Open MPI to be installed first.  By default, the runner
finds Open MPI tools on `PATH`.  Environment or make variables may
override tool paths.

The target must live primarily in `test/mpi-abi/Makefile.am`, with a
top-level convenience `check-abi` target.

The target must use:

* installed `mpicc_abi` for standard ABI C compile and link tests;
* installed `mpirun` for runtime tests;
* installed standard ABI headers and libraries;
* one executable per generated runtime test case, because MPI runtime
  state after a failed MPI call is not generally reusable.

CI-friendly runtime tests should use one or two ranks by default.
Tests requiring larger scale, unusual launch support, or specialized
resources must be optional or separately classified.

### make check-abi-mpich

`make check-abi-mpich` is an optional MPICH compatibility ABI target.

The target must support both directions:

* compile with MPICH and run with Open MPI's `libmpi_abi`;
* compile with Open MPI's `mpicc_abi` and run with MPICH.

The runner should search `PATH` by default and allow explicit override
variables for both Open MPI and MPICH compiler wrappers and launchers.
It must also allow library path and launcher environment overrides,
because cross-implementation tests need to control which implementation
provides `libmpi_abi` at runtime.

Cross-direction runtime launches must sanitize the platform runtime
library path.  The runner should replace the selected loader variable
(`LD_LIBRARY_PATH` on Linux, `DYLD_LIBRARY_PATH` on macOS) with the
run-side implementation's library directory instead of preserving stale
entries from the invoking shell.

When MPICH mode is enabled, the suite should also compare Open MPI's
standard ABI header against MPICH's ABI header semantically where
possible.

## Runner and Generated Artifacts

Python is the preferred language for manifest generation, source
generation, compile/run orchestration, and report production.  The
runner must use `$(PYTHON)` from the Open MPI build when invoked by
Makefile targets.

Generated ABI test source files must not be checked into git.  The
generator, templates, hand-written tests, and metadata inputs are
checked in.  Generated sources are emitted into the build tree.  The
runner must provide a debugging mode that preserves or prints generated
test sources for failed cases.

Generated sources, executables, logs, and reports must be removed by the
appropriate Automake clean targets unless debug-preservation mode is
enabled.

The test runner may operate outside Automake `check_PROGRAMS` / `TESTS`
for `make check-abi` and `make check-abi-mpich`.  Normal fast tests
included in `make check` should use existing Open MPI test conventions
where practical.

The runner must collect failures and skips and emit a final summary
rather than stopping at the first failure.

## Python Requirements

This effort should use one requirements file for ABI tests:

* `test/mpi-abi/requirements.txt`

Open MPI currently has `docs/requirements.txt`, but no top-level
requirements file.  If Open MPI adopts a top-level Python requirements
entry point, it should include both documentation and ABI test
requirements with `-r docs/requirements.txt` and
`-r test/mpi-abi/requirements.txt`.

The ABI test requirements should be kept minimal.  Prefer the Python
standard library where practical.

## Tool Discovery and Overrides

By default, installed tools are found on `PATH`.

Open MPI override variables should include at least:

* `OMPI_ABI_TEST_MPICC_ABI`
* `OMPI_ABI_TEST_MPIRUN`

MPICH cross-test override variables should include at least:

* `MPICH_ABI_TEST_MPICC`
* `MPICH_ABI_TEST_MPIRUN`

The runner may define additional variables for include paths, library
paths, launcher arguments, temporary directories, and rank counts.
Library path handling must account for both Linux and macOS runtime
loader variables.

## Reports

The suite must emit:

* a machine-readable JSON coverage and results report in the build tree;
* a human-readable text summary.

Every skip reason must be stable and machine-readable so CI can track
skip counts over time.

The report must include:

* total metadata API entries;
* APIs implemented, skipped, unsupported, and not yet tested;
* ABI constants covered and skipped;
* C, `mpif.h`, `use mpi`, and `use mpi_f08` coverage;
* rank count used for runtime tests;
* compiler wrapper and launcher paths;
* temporary directory paths;
* optional symbol-table diagnostics when available.

## Symbol and Header Checks

Compile/link probes are required for symbol validation.  Optional
symbol-table diagnostics should use platform tools when available:

* Linux: `nm` and/or `readelf`;
* macOS: `nm` and/or `otool`.

Symbol-table diagnostics are supplementary and must not be the only way
ABI symbol availability is tested.

Open MPI's generated standard ABI header must be checked semantically
against the ABI metadata.  When MPICH cross-testing is enabled, Open
MPI's ABI header should also be semantically compared with MPICH's ABI
header.

## Temporary Files

`make check-abi` requires an installed Open MPI and may create temporary
files and directories for MPI-IO, dynamic process, and generated-source
tests.  Temporary paths should default to the build tree or a normal
system temporary directory and be overrideable.

## Failure Policy

Hard failures:

* implemented ABI functionality diverges from MPI standard ABI
  behavior;
* compile/link/runtime tests for implemented ABI functionality fail;
* generated headers or constants disagree with authority metadata;
* new metadata entries are not classified;
* completed-suite coverage contains `test_not_written_yet`.

Skips and setup failures:

* standard ABI support is disabled in this build;
* the relevant Open MPI language binding is not configured;
* the configured build lacks a required optional feature;
* Open MPI explicitly does not implement the ABI functionality yet;
* optional MPICH compatibility prerequisites are not available.

`make check-abi-mpich` is an explicit request for MPICH compatibility
results.  Missing Open MPI or MPICH tools are setup failures for that
target rather than skips; the runner should report the missing
prerequisite and point users at the MPI ABI test README for setup
instructions.

## Completion Criteria

This branch is complete when:

* `make check` includes fast ABI checks and skips cleanly when standard
  ABI support is disabled;
* `make check-abi` validates installed Open MPI ABI behavior for all
  implemented C standard ABI functionality;
* `make check-abi` includes conditional exhaustive regression coverage
  for `mpif.h`, `use mpi`, and `use mpi_f08`;
* `make check-abi` includes Fortran ABI functionality checks for
  implemented `use mpi_f08` ABI behavior and skips unimplemented
  standard ABI functionality explicitly;
* `make check-abi-mpich` supports optional MPICH compatibility
  testing in both compile/run directions;
* every API and ABI constant from the authority metadata is classified;
* no implemented ABI functionality diverges from the MPI standard ABI
  metadata;
* `test_not_written_yet` is not present for implemented ABI
  functionality;
* JSON and text reports are emitted and suitable for CI tracking.
