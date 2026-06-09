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

- [x] Add `test/mpi-abi/` with Open MPI-style source headers.
- [x] Add `test/mpi-abi/Makefile.am`.
- [x] Add `test/mpi-abi/requirements.txt`.
- [x] Add `test/mpi-abi/README.md` documenting the test tiers and
      runner variables.
- [x] Add `test/mpi-abi/Makefile` to the Autoconf-generated file list.
- [x] Add `test/mpi-abi` to the appropriate `test/Makefile.am`
      `SUBDIRS` / `DIST_SUBDIRS` entries.
- [x] Gate `test/mpi-abi` build participation on OMPI project support
      where appropriate.
- [x] Add a top-level convenience `check-abi` target that dispatches to
      `test/mpi-abi`.
- [x] Add a top-level convenience `check-abi-cross` target that
      dispatches to `test/mpi-abi`.
- [x] Ensure the ABI test files are included in `make dist`.
- [x] Ensure `specs/mpi-abi-tests/` is included in `make dist` if the
      repository does not already distribute `specs/`.
- [x] Ensure generated test sources are not checked into git and are not
      distributed as source artifacts.
- [x] Add `clean-local` / `distclean-local` coverage for generated
      sources, executables, logs, and reports.
- [x] Ensure targets skip with a stable machine-readable reason when
      Open MPI is configured without standard ABI support.

## Phase 2: Runner Skeleton

- [x] Add the Python runner entry point under `test/mpi-abi/`.
- [x] Add checked-in templates for generated C test cases.
- [x] Add checked-in templates for generated Fortran test cases.
- [x] Add a checked-in location for hand-written test sources for cases
      that cannot be generated cleanly.
- [x] Invoke the runner through `$(PYTHON)` from Makefile targets.
- [x] Implement command modes for:
      `manifest`, `coverage`, `check-fast`, `check-abi`, and
      `check-abi-cross`.
- [x] Implement tool discovery from `PATH`.
- [x] Implement Open MPI override variables:
      `OMPI_ABI_TEST_MPICC_ABI` and `OMPI_ABI_TEST_MPIRUN`.
- [x] Implement MPICH override variables:
      `MPICH_ABI_TEST_MPICC` and `MPICH_ABI_TEST_MPIRUN`.
- [x] Implement rank-count override variables for one-rank and two-rank
      tests.
- [x] Implement launcher argument override variables.
- [x] Implement include-path and library-path override variables.
- [x] Implement Linux and macOS runtime loader environment handling for
      cross-implementation library selection.
- [x] Implement temporary-directory selection and cleanup.
- [x] Implement a debug mode that preserves generated test sources and
      build artifacts.
- [x] Make the runner collect failures and skips, then emit a final
      summary instead of failing fast.

## Phase 3: Metadata Loading and Manifest

- [x] Load `docs/mpi-standard-apis.json`.
- [x] Load `docs/mpi-standard-abi.json`.
- [x] Validate that the selected metadata files match the expected
      branch-local MPI standard metadata version.
- [x] Validate the JSON shape expected by the runner.
- [x] Detect whether the configured build has standard ABI support
      enabled.
- [ ] Detect configured optional MPI feature support needed for
      classification.
      This remains open because this branch has no valid
      `AM_CONDITIONAL` that maps an MPI API family to
      `unsupported_by_build`.  Standard ABI support and Fortran binding
      layers are detected separately.
- [x] Generate a manifest entry for every API in the authority metadata.
- [x] Generate a manifest entry for every ABI constant in the authority
      metadata.
- [x] Classify each entry with one of:
      `implemented`, `not_implemented`, `not_in_standard_abi`,
      `unsupported_by_build`, or `unsupported_by_open_mpi`.
- [x] Record `test_not_written_yet` as a separate test-status value
      instead of a classification value.
- [x] Record stable machine-readable skip reasons.
- [x] Record language coverage fields for C, `mpif.h`, `use mpi`, and
      `use mpi_f08`.
- [x] Record API-family, rank-count, and optional-feature requirements.
- [x] Detect newly added metadata entries that have not been classified.
- [x] Add a manifest JSON output file in the build tree.

## Phase 4: Reporting and Coverage Policy

- [x] Emit a machine-readable JSON coverage and results report in the
      build tree.
- [x] Emit a human-readable text summary.
- [x] Report total API metadata entries.
- [x] Report implemented, skipped, unsupported, not implemented, and not
      yet tested APIs.
- [x] Report ABI constants covered and skipped.
- [x] Report C, `mpif.h`, `use mpi`, and `use mpi_f08` coverage.
- [x] Report compiler wrapper, launcher, rank-count, and temporary
      directory paths.
- [x] Report include paths, library paths, and launcher arguments.
- [x] Report optional symbol-table diagnostics when available.
- [x] During development, allow `test_not_written_yet` entries but
      report them clearly.
- [x] Add a completion-gate mode where `test_not_written_yet` is a hard
      failure for implemented ABI functionality.

## Phase 5: Fast `make check` Tests

- [x] Add fast ABI tests that can run without installed Open MPI,
      `mpicc_abi`, or `mpirun`.
- [x] Add metadata validation tests.
- [x] Add manifest generation sanity tests.
- [x] Add semantic checks for generated standard ABI header constants
      when available in-tree.
      This is a fast subset that checks numeric C-header constants
      against `docs/mpi-standard-abi.json`; exhaustive semantic
      comparisons remain in Phase 7.
- [x] Add converter source-contract checks that do not launch MPI jobs.
      These are fast source-contract checks for the ABI converter
      sources, generated converter header, and converter source file;
      behavioral converter tests remain in Phase 8.
- [x] Add in-tree Fortran ABI helper tests where possible and gated on
      configured Fortran support.
      These validate `mpif.h` and `use mpi_f08` ABI helper source
      coverage, skip `use mpi` with a stable shared-with-`mpif.h`
      reason, and skip binding layers that `configure` explicitly
      disabled.
- [x] Ensure all fast tests skip cleanly when standard ABI support is
      disabled.
- [x] Wire fast tests into normal `make check` where practical.

## Phase 6: Installed `make check-abi` Framework

- [x] Require installed Open MPI tools to be available through `PATH` or
      override variables.
- [x] Skip with a stable machine-readable reason when installed Open MPI
      standard ABI tools are unavailable.
- [x] Use installed `mpicc_abi` for standard ABI C compile/link tests.
      The Phase 6 framework uses a seed set of generated C ABI probes;
      exhaustive generated C probes remain in Phase 7 and later.
- [x] Use installed `mpirun` for runtime tests.
- [x] Verify installed standard ABI headers are used.
      Generated C probes require `MPI_H_ABI` from the included
      `mpi.h`, so a non-standard-ABI header fails at compile time.
- [x] Verify installed `libmpi_abi` is linked.
      The runner checks wrapper flags and inspects each generated
      executable's dynamic-library dependencies.
- [x] Build one executable per generated runtime test case.
- [x] Run one-rank tests by default for APIs that do not require
      communication.
- [x] Run two-rank tests by default for point-to-point and collective
      APIs.
      Seed two-rank cases cover point-to-point and collective framework
      paths; broader API family expansion remains in Phase 9 and later.
- [ ] Support optional tests that require larger scale or unusual launch
      support.
- [x] Support launcher argument overrides for CI and site-specific
      launch requirements.
- [x] Store generated sources, executables, logs, and reports in the
      build tree or selected temporary directory.

## Phase 7: C ABI Header, Constant, and Symbol Tests

- [x] Generate compile probes for every C standard ABI API prototype.
      The installed test runner parses the installed standard ABI
      `mpi.h` and generates an aggregate typed function-pointer probe
      for every declared `MPI_*` and `PMPI_*` prototype.
- [x] Generate link probes for every implemented C standard ABI API.
      The aggregate C header probe links an executable with
      `mpicc_abi`, forcing references to all parsed standard ABI
      function entry points.
- [x] Confirm expected `MPI_*` symbols are reachable through
      `libmpi_abi`.
      The aggregate C header probe and optional symbol-table check
      validate the `MPI_*` side of the standard ABI surface.  The
      symbol-table diagnostic requires defined symbols and does not count
      undefined references as exports.
- [x] Confirm expected `PMPI_*` symbols are reachable through
      `libmpi_abi`.
      The aggregate C header probe and optional symbol-table check
      validate the `PMPI_*` side of the standard ABI surface.  The
      symbol-table diagnostic requires defined symbols and does not count
      undefined references as exports.
- [x] Confirm non-ABI APIs are not accidentally exposed as standard ABI
      entry points where this can be tested portably.
      The installed header check verifies known non-ABI C entry points
      such as `*_c2f`, `*_f2c`, and legacy `MPI_Attr_*` routines are
      not declared by the standard ABI header.
- [x] Semantically compare Open MPI's generated standard ABI `mpi.h`
      against `docs/mpi-standard-abi.json`.
      Fast header-constant checks compare numeric C-header constants
      against the ABI metadata; installed checks verify the installed
      header is the standard ABI header used by `mpicc_abi`.
- [x] Compare Open MPI's generated standard ABI `mpi.h` against API
      metadata and MPI-standard-derived ISO C signatures.
      Installed header checks compare implemented C API metadata names
      against the installed standard ABI header declarations and compare
      header prototype signatures against the MPI-standard-derived ISO C
      procedure signatures used by the binding generator, excluding APIs
      that are expected to remain outside the standard ABI C header.
- [x] Add optional Linux symbol-table diagnostics with `nm` and/or
      `readelf`.
      The installed runner uses `nm -g` when available and reports a
      stable skip when symbol-table diagnostics are unavailable.
- [x] Add optional macOS symbol-table diagnostics with `nm` and/or
      `otool`.
      The installed runner uses `nm -g` when available and reports a
      stable skip when symbol-table diagnostics are unavailable.

## Phase 8: C ABI Converter Tests

- [x] Drive predefined-handle and error-class probe coverage from ABI
      metadata and the installed standard ABI header.
      The runner derives the checked C handle constants, Fortran
      datatype constants, predefined error handlers, and `MPI_ERR_*`
      error classes from `docs/mpi-standard-abi.json`, verifies that
      the installed standard ABI `mpi.h` declares them, and emits
      metadata-driven C probe checks.  Each generated family has an
      explicit nonempty-generation preflight and an exact expected-count
      guard so metadata/header drift cannot silently become a vacuous or
      eroded-coverage pass.
- [x] Test ABI-to-OMPI and OMPI-to-ABI conversion for predefined
      communicators.
      Installed C converter probes round-trip `MPI_COMM_NULL`,
      `MPI_COMM_WORLD`, and `MPI_COMM_SELF` through `MPI_Comm_toint` /
      `MPI_Comm_fromint` and their PMPI equivalents.
- [x] Test conversion for predefined datatypes.
      Installed C converter probes round-trip all predefined C datatypes
      declared by the standard ABI header and `MPI_DATATYPE_NULL`
      through `MPI_Type_toint` / `MPI_Type_fromint` and their PMPI
      equivalents.
- [x] Test conversion for configured optional Fortran datatypes.
      The Fortran datatype probe is gated on configure-detected Fortran
      bindings and round-trips every declared core, sized, and paired
      Fortran datatype macro through `MPI_Type_toint` /
      `MPI_Type_fromint` and their PMPI equivalents.
- [ ] Test unavailable optional Fortran datatype behavior.
- [x] Test conversion for predefined groups.
      Installed C converter probes cover `MPI_GROUP_NULL` and
      `MPI_GROUP_EMPTY`.
- [x] Test conversion for predefined requests and messages.
      Installed C converter probes cover `MPI_REQUEST_NULL`,
      `MPI_MESSAGE_NULL`, `MPI_MESSAGE_NO_PROC`, plus dynamic request
      and message handles.
- [x] Test conversion for predefined windows.
      Installed C converter probes cover `MPI_WIN_NULL` and a dynamic
      window handle.
- [x] Test conversion for predefined files.
      Installed C converter probes cover `MPI_FILE_NULL` and a dynamic
      MPI-IO file handle.
- [x] Test conversion for predefined infos.
      Installed C converter probes cover `MPI_INFO_NULL`, `MPI_INFO_ENV`,
      and a dynamic info handle.
- [x] Test conversion for predefined error handlers.
      Installed C converter probes round-trip every metadata-defined
      `MPI_Errhandler` constant declared by the standard ABI header.
- [x] Test conversion for error classes and error codes.
      Installed C converter probes check every metadata-defined
      `MPI_ERR_*` error class declared by the standard ABI header,
      excluding the `MPI_ERR_LASTCODE` sentinel, and a dynamic
      error-code-to-error-class path.
- [x] Test conversion for keyvals and attribute-related constants.
      Installed C converter probes cover communicator, datatype, and
      window keyval creation/free paths using the standard ABI callback
      constants.  The probes attach attributes, duplicate communicators
      and datatypes, verify null-copy and duplicate-copy behavior where
      MPI provides duplicate operations, directly check window callback
      sentinel values, and delete/free the attributes and objects.
- [x] Test conversion for status fields.
      Installed C converter probes check source/tag/count conversion
      through a real `MPI_Sendrecv` status.
- [x] Test `*_toint` and `*_fromint` ABI functions.
      Installed C converter probes exercise all standard ABI public
      handle `*_toint` / `*_fromint` families and PMPI equivalents.
- [x] Test null handles, empty handles, ignore sentinels, and status
      ignore sentinels.
      Installed C converter probes cover null handles, `MPI_GROUP_EMPTY`,
      `MPI_STATUS_IGNORE`, `MPI_STATUSES_IGNORE`, and `MPI_IN_PLACE`.

## Phase 9: C ABI Runtime API Family Tests

- [x] Add a generated installed C runtime API probe framework.
      Runtime API probes are generated from a checked-in case table,
      validated against API metadata and the installed standard ABI
      header, compiled as one executable per logical probe, and launched
      independently so a failed MPI job cannot affect later probes.
- [x] Generate runtime tests for initialization and finalization APIs.
      Installed C runtime probes cover `MPI_Init`, `MPI_Init_thread`,
      `MPI_Initialized`, `MPI_Finalized`, `MPI_Finalize`,
      `MPI_Query_thread`, and `MPI_Is_thread_main` in separate
      executables where required by MPI initialization semantics.
- [ ] Generate runtime tests for sessions APIs.
      This remains open until all in-scope standard ABI session runtime
      APIs are covered or explicitly skipped.
- [x] Generate runtime tests for core non-callback session APIs.
      Installed C runtime probes cover session lifecycle, pset
      discovery, pset info lookup, session info lookup, session
      errhandler get/set/call with predefined handlers, and dynamic
      session `toint` / `fromint` round trips.
- [x] Generate runtime tests for session buffer attach, detach, and
      flush APIs.
      Installed C runtime probes cover `MPI_Session_attach_buffer`,
      `MPI_Session_flush_buffer`, `MPI_Session_iflush_buffer`, and
      `MPI_Session_detach_buffer`.
- [x] Explicitly defer session error-handler callback behavior to
      Phase 10.
- [x] Keep non-standard-ABI C `MPI_Session_c2f` / `MPI_Session_f2c`
      regression coverage outside the installed standard ABI C runtime
      probes.
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
- [ ] Verify `make distcheck` passes with MPI ABI test files included.
- [ ] Verify `make check-abi` passes against an installed Open MPI.
- [ ] Verify `make check-abi-cross` passes when MPICH is available or
      skips cleanly when MPICH is unavailable.
- [ ] Verify a distribution tarball can configure, build, install, and
      run `make check-abi` against the installed tarball build.
- [ ] Verify a distribution tarball can run `make check-abi-cross` when
      MPICH is available or skip cleanly when MPICH is unavailable.
- [ ] Verify JSON and text reports are emitted and suitable for CI
      tracking.
