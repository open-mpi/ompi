<!--
  Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.

  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# MPI ABI test suite tasks

This task list implements `specs/mpi-abi-tests/spec.md`.  The work is
organized as phases that can become multiple commits on this branch, but
the branch is not complete until all completion-gate tasks pass.

## Phase 1: Test Directory and Build Plumbing

- [ ] Add `test/mpi-abi/` with Open MPI-style source headers.
- [ ] Add `test/mpi-abi/Makefile.am`.
- [ ] Add `test/mpi-abi/requirements.txt`.
- [ ] Add `test/mpi-abi/README.md` documenting the test tiers and
      runner variables.
- [ ] Add `test/mpi-abi/Makefile` to the Autoconf-generated file list.
- [ ] Add `test/mpi-abi` to the appropriate `test/Makefile.am`
      `SUBDIRS` / `DIST_SUBDIRS` entries.
- [ ] Gate `test/mpi-abi` build participation on OMPI project support
      where appropriate.
- [ ] Add a top-level convenience `check-abi` target that dispatches to
      `test/mpi-abi`.
- [ ] Add a top-level convenience `check-abi-cross` target that
      dispatches to `test/mpi-abi`.
- [ ] Ensure the ABI test files are included in `make dist`.
- [ ] Ensure `specs/mpi-abi-tests/` is included in `make dist` if the
      repository does not already distribute `specs/`.
- [ ] Ensure generated test sources are not checked into git and are not
      distributed as source artifacts.
- [ ] Add `clean-local` / `distclean-local` coverage for generated
      sources, executables, logs, and reports.
- [ ] Ensure targets skip with a stable machine-readable reason when
      Open MPI is configured without standard ABI support.

## Phase 2: Runner Skeleton

- [ ] Add the Python runner entry point under `test/mpi-abi/`.
- [ ] Add checked-in templates for generated C test cases.
- [ ] Add checked-in templates for generated Fortran test cases.
- [ ] Add checked-in hand-written test sources for cases that cannot be
      generated cleanly.
- [ ] Invoke the runner through `$(PYTHON)` from Makefile targets.
- [ ] Implement command modes for:
      `manifest`, `coverage`, `check-fast`, `check-abi`, and
      `check-abi-cross`.
- [ ] Implement tool discovery from `PATH`.
- [ ] Implement Open MPI override variables:
      `OMPI_ABI_TEST_MPICC_ABI` and `OMPI_ABI_TEST_MPIRUN`.
- [ ] Implement MPICH override variables:
      `MPICH_ABI_TEST_MPICC` and `MPICH_ABI_TEST_MPIRUN`.
- [ ] Implement rank-count override variables for one-rank and two-rank
      tests.
- [ ] Implement launcher argument override variables.
- [ ] Implement include-path and library-path override variables.
- [ ] Implement Linux and macOS runtime loader environment handling for
      cross-implementation library selection.
- [ ] Implement temporary-directory selection and cleanup.
- [ ] Implement a debug mode that preserves generated test sources and
      build artifacts.
- [ ] Make the runner collect failures and skips, then emit a final
      summary instead of failing fast.

## Phase 3: Metadata Loading and Manifest

- [ ] Load `docs/mpi-standard-apis.json`.
- [ ] Load `docs/mpi-standard-abi.json`.
- [ ] Validate that the selected metadata files match the expected
      branch-local MPI standard metadata version.
- [ ] Validate the JSON shape expected by the runner.
- [ ] Detect whether the configured build has standard ABI support
      enabled.
- [ ] Detect configured optional MPI feature support needed for
      classification.
- [ ] Generate a manifest entry for every API in the authority metadata.
- [ ] Generate a manifest entry for every ABI constant in the authority
      metadata.
- [ ] Classify each entry with one of:
      `implemented`, `not_implemented`, `not_in_standard_abi`,
      `unsupported_by_build`, `unsupported_by_open_mpi`, or
      `test_not_written_yet`.
- [ ] Record stable machine-readable skip reasons.
- [ ] Record language coverage fields for C, `mpif.h`, `use mpi`, and
      `use mpi_f08`.
- [ ] Record API-family, rank-count, and optional-feature requirements.
- [ ] Detect newly added metadata entries that have not been classified.
- [ ] Add a manifest JSON output file in the build tree.

## Phase 4: Reporting and Coverage Policy

- [ ] Emit a machine-readable JSON coverage and results report in the
      build tree.
- [ ] Emit a human-readable text summary.
- [ ] Report total API metadata entries.
- [ ] Report implemented, skipped, unsupported, not implemented, and not
      yet tested APIs.
- [ ] Report ABI constants covered and skipped.
- [ ] Report C, `mpif.h`, `use mpi`, and `use mpi_f08` coverage.
- [ ] Report compiler wrapper, launcher, rank-count, and temporary
      directory paths.
- [ ] Report include paths, library paths, and launcher arguments.
- [ ] Report optional symbol-table diagnostics when available.
- [ ] During development, allow `test_not_written_yet` entries but
      report them clearly.
- [ ] Add a completion-gate mode where `test_not_written_yet` is a hard
      failure for implemented ABI functionality.

## Phase 5: Fast `make check` Tests

- [ ] Add fast ABI tests that can run without installed Open MPI,
      `mpicc_abi`, or `mpirun`.
- [ ] Add metadata validation tests.
- [ ] Add manifest generation sanity tests.
- [ ] Add semantic checks for generated standard ABI header constants
      when available in-tree.
- [ ] Add converter unit tests that do not launch MPI jobs.
- [ ] Add in-tree Fortran ABI helper tests where possible and gated on
      configured Fortran support.
- [ ] Ensure all fast tests skip cleanly when standard ABI support is
      disabled.
- [ ] Wire fast tests into normal `make check` where practical.

## Phase 6: Installed `make check-abi` Framework

- [ ] Require installed Open MPI tools to be available through `PATH` or
      override variables.
- [ ] Skip with a stable machine-readable reason when installed Open MPI
      standard ABI tools are unavailable.
- [ ] Use installed `mpicc_abi` for standard ABI C compile/link tests.
- [ ] Use installed `mpirun` for runtime tests.
- [ ] Verify installed standard ABI headers are used.
- [ ] Verify installed `libmpi_abi` is linked.
- [ ] Build one executable per generated runtime test case.
- [ ] Run one-rank tests by default for APIs that do not require
      communication.
- [ ] Run two-rank tests by default for point-to-point and collective
      APIs.
- [ ] Support optional tests that require larger scale or unusual launch
      support.
- [ ] Support launcher argument overrides for CI and site-specific
      launch requirements.
- [ ] Store generated sources, executables, logs, and reports in the
      build tree or selected temporary directory.

## Phase 7: C ABI Header, Constant, and Symbol Tests

- [ ] Generate compile probes for every C standard ABI API prototype.
- [ ] Generate link probes for every implemented C standard ABI API.
- [ ] Confirm expected `MPI_*` symbols are reachable through
      `libmpi_abi`.
- [ ] Confirm expected `PMPI_*` symbols are reachable through
      `libmpi_abi`.
- [ ] Confirm non-ABI APIs are not accidentally exposed as standard ABI
      entry points where this can be tested portably.
- [ ] Semantically compare Open MPI's generated standard ABI `mpi.h`
      against `docs/mpi-standard-abi.json`.
- [ ] Semantically compare Open MPI's generated standard ABI `mpi.h`
      against `docs/mpi-standard-apis.json`.
- [ ] Add optional Linux symbol-table diagnostics with `nm` and/or
      `readelf`.
- [ ] Add optional macOS symbol-table diagnostics with `nm` and/or
      `otool`.

## Phase 8: C ABI Converter Tests

- [ ] Test ABI-to-OMPI and OMPI-to-ABI conversion for predefined
      communicators.
- [ ] Test conversion for predefined datatypes.
- [ ] Test conversion for optional Fortran datatypes and unavailable
      datatype behavior.
- [ ] Test conversion for predefined groups.
- [ ] Test conversion for predefined requests and messages.
- [ ] Test conversion for predefined windows.
- [ ] Test conversion for predefined files.
- [ ] Test conversion for predefined infos.
- [ ] Test conversion for predefined error handlers.
- [ ] Test conversion for error classes and error codes.
- [ ] Test conversion for keyvals and attribute-related constants.
- [ ] Test conversion for status fields.
- [ ] Test `*_toint` and `*_fromint` ABI functions.
- [ ] Test null handles, empty handles, ignore sentinels, and status
      ignore sentinels.

## Phase 9: C ABI Runtime API Family Tests

- [ ] Generate runtime tests for initialization and finalization APIs.
- [ ] Generate runtime tests for sessions APIs.
- [ ] Generate runtime tests for communicator and group APIs.
- [ ] Generate runtime tests for topology APIs.
- [ ] Generate runtime tests for point-to-point APIs.
- [ ] Generate runtime tests for collectives.
- [ ] Generate runtime tests for persistent collectives.
- [ ] Generate runtime tests for datatype creation and introspection.
- [ ] Generate runtime tests for reductions and user-defined operations.
- [ ] Generate runtime tests for attributes and keyvals.
- [ ] Generate runtime tests for error handlers.
- [ ] Generate runtime tests for generalized requests.
- [ ] Generate runtime tests for RMA/window APIs.
- [ ] Generate runtime tests for MPI-IO APIs.
- [ ] Generate runtime tests for dynamic process management APIs.
- [ ] Generate runtime tests for MPI_T APIs.
- [ ] Generate runtime tests for big-count variants.
- [ ] Ensure all output handles, statuses, errors, and callback
      arguments are checked for standard ABI values.

## Phase 10: Callback and Lifetime Tests

- [ ] Test communicator attribute copy/delete callback wrapping.
- [ ] Test datatype attribute copy/delete callback wrapping.
- [ ] Test window attribute copy/delete callback wrapping.
- [ ] Test communicator error handler callback ABI values.
- [ ] Test file error handler callback ABI values.
- [ ] Test session error handler callback ABI values.
- [ ] Test window error handler callback ABI values.
- [ ] Test user-defined reduction callback datatype conversion.
- [ ] Test generalized request callback behavior.
- [ ] Test datarep callback behavior where Open MPI support exists.
- [ ] Test MPI_T event callback behavior where Open MPI support exists.
- [ ] Test nonblocking and persistent operations that retain converted
      arrays until completion or request release.
- [ ] Test cleanup paths for callback wrappers and retained converted
      arrays.

## Phase 11: Fortran Regression and ABI Tests

- [ ] Detect configured `mpif.h` support.
- [ ] Detect configured `use mpi` support.
- [ ] Detect configured `use mpi_f08` support.
- [ ] Generate exhaustive `mpif.h` regression tests for APIs supported
      by the configured build.
- [ ] Generate exhaustive `use mpi` regression tests for APIs supported
      by the configured build.
- [ ] Generate exhaustive `use mpi_f08` regression tests for APIs
      supported by the configured build.
- [ ] Test Fortran `MPI_Abi_get_version`.
- [ ] Test Fortran `MPI_Abi_get_info`.
- [ ] Test Fortran `MPI_Abi_get_fortran_info`.
- [ ] Test Fortran `MPI_Abi_set_fortran_info`.
- [ ] Test Fortran `MPI_Abi_get_fortran_booleans`.
- [ ] Test Fortran `MPI_Abi_set_fortran_booleans`.
- [ ] Test `use mpi_f08` ABI functionality implemented by Open MPI.
- [ ] Explicitly skip unimplemented standard Fortran ABI functionality
      with stable machine-readable reasons.
- [ ] Test Fortran logical size and true/false value reporting.
- [ ] Test optional Fortran datatype size and unavailable-type behavior.

## Phase 12: MPICH Cross-Implementation Tests

- [ ] Add optional `make check-abi-cross` runner mode.
- [ ] Discover MPICH compiler wrapper and launcher from `PATH`.
- [ ] Honor MPICH override variables.
- [ ] Honor library path and launcher environment overrides for Open MPI
      and MPICH.
- [ ] Select Linux or macOS runtime loader variables as appropriate.
- [ ] Compile with MPICH and run with Open MPI's `libmpi_abi`.
- [ ] Compile with Open MPI's `mpicc_abi` and run with MPICH.
- [ ] Semantically compare Open MPI and MPICH standard ABI headers when
      MPICH mode is enabled.
- [ ] Report MPICH tool paths and versions.
- [ ] Skip cross tests with stable machine-readable reasons when MPICH
      tools are unavailable.

## Phase 13: Completion Gate

- [ ] Enable hard failure for unclassified metadata entries.
- [ ] Enable hard failure for `test_not_written_yet` on implemented ABI
      functionality.
- [ ] Verify every API and ABI constant from the authority metadata is
      classified.
- [ ] Verify all implemented C standard ABI functionality is tested.
- [ ] Verify all configured Fortran binding layers have exhaustive
      regression coverage.
- [ ] Verify implemented `use mpi_f08` ABI functionality is tested.
- [ ] Verify unimplemented standard ABI functionality is explicitly
      skipped.
- [ ] Verify `make check` passes with ABI support enabled.
- [ ] Verify `make check` skips cleanly with ABI support disabled.
- [ ] Verify `make check-abi` passes against an installed Open MPI.
- [ ] Verify `make check-abi-cross` passes when MPICH is available or
      skips cleanly when MPICH is unavailable.
- [ ] Verify JSON and text reports are emitted and suitable for CI
      tracking.
