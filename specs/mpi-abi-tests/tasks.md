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

## Reviewable Implementation Chunks

Use these chunks to implement larger pieces of the remaining work while
keeping each commit reviewable by another LLM or human reviewer.  Each
chunk should update the relevant phase checkboxes and notes, preserve
one executable per runtime test case, and include at least syntax checks,
`git diff --check`, `check-fast`, and installed `check-abi` when the
chunk adds installed runtime probes.

- [x] Chunk 9A: Add runtime coverage audit and close current
      point-to-point seed coverage.
      Add a runner audit that reports implemented standard ABI C APIs
      not covered by compile/link, converter, runtime, callback, or
      explicit skip/defer logic.  Include the current blocking,
      nonblocking, probe, and persistent point-to-point seed probes in
      the committed task state.
- [x] Chunk 9B: Complete remaining point-to-point and request lifecycle
      probes.
      Cover buffered, synchronous, ready, sendrecv, matched-probe,
      message receive, partitioned communication, request wait/test
      variants, request status, request free, and cancellation where it
      can be tested without intentionally corrupting later MPI state.
- [x] Chunk 9C: Add topology and neighborhood-collective probes.
      Cover Cartesian, graph, distributed-graph, topology query/helper
      APIs, and then neighborhood collectives that need topology
      communicators.  Reviewers should be able to verify real topology
      metadata and data movement, not just successful return codes.
- [x] Chunk 9D: Add ordinary and persistent collective probes.
      Cover blocking, nonblocking, persistent, reduce-scatter, scan, and
      exscan families with two-rank data validation.  User-defined
      reduction callbacks remain Phase 10 unless implemented in the
      same callback-focused chunk.
- [x] Chunk 9E: Add datatype, status, packing, and predefined operation
      probes.
      Cover datatype constructors, commit/free, introspection, extents,
      names, F90 constructors, pack/unpack, external packing, status
      helpers, predefined reduction operation behavior, and predefined
      `MPI_Op` handle APIs.  Dynamic operation free remains Phase 10
      because a freeable `MPI_Op` requires `MPI_Op_create` callback
      coverage.
      Installed runtime probes now include the focused
      `MPI_Type_get_contents` coverage as well as output validation for
      the checked datatype/status/packing and predefined-operation
      paths.
- [x] Chunk 9F: Add RMA/window and memory helper probes.
      Cover window creation/allocation/dynamic attach, window metadata,
      active/passive synchronization, one-sided operations, atomics,
      request-based RMA, memory allocation/free, and address helpers,
      gated on configured RMA support.
- [x] Chunk 9G: Add MPI-IO runtime probes.
      Cover file lifecycle, views, info, position, size, atomicity,
      blocking I/O, nonblocking I/O, split-collective I/O,
      shared-file-pointer I/O, ordered I/O, and predefined file error
      handler behavior, gated on configured MPI-IO support.
- [x] Chunk 9H: Add dynamic process and name-service probes.
      Cover open/close port, publish/lookup/unpublish name, spawn,
      spawn_multiple, get-parent, connect/accept, and disconnect in
      separate executables with explicit launcher timeout handling and
      stable skips for unsupported launch environments.
- [x] Chunk 9I: Add MPI_T, big-count, miscellaneous, and final C
      runtime closure.
      Cover MPI_T non-callback APIs where implemented, big-count
      variants using small counts, timing, utility APIs, explicit skips
      for APIs such as `MPI_Abort`, and any remaining implemented C APIs
      reported by the Phase 9 audit.
      The installed metadata/header audit now reports no missing
      implemented C runtime APIs for Phase 9.  The only uncovered
      implemented C APIs are explicitly skipped or deferred with stable
      reasons: `MPI_Abort`, `MPI_Comm_join`, and callback-dependent
      Phase 10 APIs.
- [x] Chunk 10A: Add callback coverage audit and attribute callback
      probes.
      Cover communicator, datatype, window, and legacy attribute/keyval
      callback paths with copy/delete side-effect checks and cleanup
      validation.
      Installed callback probes cover the declared communicator,
      datatype, and window attribute/keyval APIs.  The callback audit
      records the legacy `MPI_Keyval_*` / `MPI_Attr_*` APIs as outside
      the installed standard ABI C header, and defers error-handler,
      user-operation, generalized-request, datarep, and MPI_T callback
      APIs to Chunk 10B with a stable reason.
- [x] Chunk 10B: Add error-handler, user operation, generalized
      request, datarep, MPI_T callback, and retained-lifetime probes.
      Cover callback argument ABI values, callback invocation,
      nonblocking/persistent retained state, cleanup, and stable skips
      for callback families Open MPI does not implement.
      Error-handler, user-operation, generalized-request, datarep, and
      MPI_T event callback registration probes are implemented.
      Retained converted-array lifetime is covered by nonblocking and
      persistent `MPI_Alltoallw` probes that overwrite the caller-side
      count, displacement, and datatype arrays after the wrapper call
      and then validate the completed data movement.
- [x] Chunk 11A: Add Fortran binding detection, coverage audit, and
      compile-only conformance probes.
      Distinguish `mpif.h`, `use mpi`, and `use mpi_f08` regression
      coverage from implemented `use mpi_f08` standard ABI coverage.
      Installed `check-abi` now records configured Fortran binding
      state, installed `mpifort` availability, an advisory per-binding
      coverage audit, and one compile-only conformance probe per
      configured binding layer.  Exhaustive per-API Fortran compile and
      runtime coverage remains in Chunk 11B and the open Phase 11 tasks
      below.
- [x] Chunk 11B: Add Fortran runtime and Fortran ABI probes.
      Cover configured Fortran runtime families, Fortran ABI helper
      routines, logical values, handles, statuses, optional datatypes,
      and explicit skips for unimplemented standard Fortran ABI
      functionality.
      Installed `check-abi` now compiles and launches isolated
      `mpif.h`, `use mpi`, and `use mpi_f08` runtime probes plus ABI
      helper probes for the six Fortran `MPI_Abi_*` routines.  The
      `use mpi_f08` runtime probe validates typed communicator,
      datatype, and status behavior.  Optional Fortran datatype
      generation is reported with a stable skip and remains open below
      until generated module-driven probes can distinguish unavailable
      optional types from true ABI divergences.
- [ ] Chunk 12A: Add MPICH cross-test infrastructure and environment
      reporting.
      Implement `check-abi-cross` discovery, overrides, loader
      selection, platform skips, tool/version reports, and combined
      per-direction summaries.
- [ ] Chunk 12B: Add MPICH cross-direction probe reuse and semantic
      comparisons.
      Run converter, runtime, and supported callback probes in both
      compile/run directions; add parsed header comparisons, wrapper
      compile/link intent checks, and optional linkage diagnostics.
- [ ] Chunk 13A: Add completion-gate audits and CI/distribution
      validation.
      Enforce completed-suite coverage for APIs and constants, callback
      and resource-sensitive coverage, PASS/FAIL/SKIP gate behavior,
      report stability, VPATH behavior, `make distcheck`, and ABI tests
      from distribution tarballs.

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
- [x] Detect configured optional MPI feature support needed for runtime
      skip decisions.
      The runner detects MPI-IO support through `OMPI_OMPIO_SUPPORT` and
      RMA support through configured OSC component Makefiles in
      `config.status`.  If the feature state cannot be observed, the
      installed probes run instead of silently skipping coverage.
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
- [x] Keep Phase 9 runtime probes isolated by executable.
      Each executed runtime test case must be compiled and run as its
      own executable.  A probe may exercise closely related APIs in one
      successful MPI lifetime, but independent cases must not share an
      executable because MPI process state after a runtime failure is
      undefined.
- [x] Add a Phase 9 runtime coverage audit.
      Report implemented standard ABI C APIs that are not covered by any
      runtime probe, not already covered by earlier compile/converter
      checks, and not explicitly deferred to Phase 10 or a later phase.
      This audit should group missing APIs by runtime work package so
      larger implementation chunks can be taken without relying on a
      reviewer to discover omissions.
- [x] Generate runtime tests for initialization and finalization APIs.
      Installed C runtime probes cover `MPI_Init`, `MPI_Init_thread`,
      `MPI_Initialized`, `MPI_Finalized`, `MPI_Finalize`,
      `MPI_Query_thread`, and `MPI_Is_thread_main` in separate
      executables where required by MPI initialization semantics.
- [x] Generate runtime tests for sessions APIs.
      Installed C runtime probes cover all currently implemented,
      standard ABI C session APIs that do not require user callbacks.
      Session error-handler callback creation remains explicitly
      deferred to Phase 10.
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
- [x] Audit remaining implemented non-callback session APIs and close or
      explicitly defer the session parent task.
      The Phase 9 runtime audit reports no remaining implemented,
      header-declared, non-callback session APIs outside the installed
      session probes.
- [x] Explicitly defer session error-handler callback behavior to
      Phase 10.
- [x] Keep non-standard-ABI C `MPI_Session_c2f` / `MPI_Session_f2c`
      regression coverage outside the installed standard ABI C runtime
      probes.
- [x] Generate runtime tests for communicator and group APIs.
      Installed C runtime probes cover the implemented communicator and
      group runtime APIs that are portable in local CI.  `MPI_Comm_join`
      is explicitly skipped because it requires a connected file
      descriptor setup that is not provided by the standard MPI launcher
      path, and callback-dependent attribute/error-handler APIs are
      deferred to Phase 10.
- [x] Generate runtime tests for communicator basic state, naming,
      info, duplication, and predefined errhandler get/set/call APIs.
- [x] Generate runtime tests for communicator creation and splitting
      APIs that are portable in two-rank local CI jobs.
- [x] Generate runtime tests for group construction, rank translation,
      comparison, and set-operation APIs.
- [x] Generate runtime tests for communicator buffer attach, detach,
      flush, and asynchronous flush APIs.
      Installed C runtime probes cover `MPI_Comm_attach_buffer`,
      `MPI_Comm_flush_buffer`, `MPI_Comm_iflush_buffer`, and
      `MPI_Comm_detach_buffer` with two-rank buffered-send traffic.
- [x] Generate runtime tests for intercommunicator creation, merging,
      comparison, and teardown APIs that are portable in local CI.
- [x] Generate runtime tests for communicator dynamic process APIs,
      including connect, accept, disconnect, spawn, spawn_multiple, and
      parent-communicator behavior.
- [x] Explicitly defer communicator attribute copy/delete callback
      wrapping to Phase 10 callback tests.
- [x] Explicitly defer communicator error-handler callback behavior to
      Phase 10 callback tests.
- [x] Generate runtime tests for topology APIs.
      Cover Cartesian, graph, and distributed-graph creation/query
      paths, plus topology helpers such as `MPI_Dims_create` and
      `MPI_Topo_test`.
- [x] Generate runtime tests for Cartesian topology APIs.
- [x] Generate runtime tests for graph topology APIs.
- [x] Generate runtime tests for distributed graph topology APIs.
- [x] Generate runtime tests for point-to-point APIs.
- [x] Generate runtime tests for blocking and nonblocking send/recv
      point-to-point APIs.
- [x] Generate runtime tests for point-to-point probe APIs.
- [x] Generate runtime tests for persistent point-to-point send/recv
      initialization APIs.
      The current installed runtime probes cover the implemented
      metadata `point_to_point` family: `MPI_Send`, `MPI_Recv`,
      `MPI_Isend`, `MPI_Irecv`, `MPI_Probe`, `MPI_Iprobe`,
      `MPI_Send_init`, and `MPI_Recv_init`.
- [x] Generate runtime tests for buffered, synchronous, and ready send
      APIs and their persistent initialization variants.
- [x] Generate runtime tests for sendrecv and nonblocking sendrecv APIs.
- [x] Generate runtime tests for matched-probe and message receive APIs.
- [x] Generate runtime tests for partitioned point-to-point APIs.
- [x] Generate runtime tests for request lifecycle, wait/test, and
      cancellation APIs used by point-to-point and persistent operations.
- [x] Generate runtime tests for collectives.
      Cover blocking and nonblocking collective APIs with real data
      movement and result validation, using two ranks unless the API
      requires a larger or feature-gated setup.
- [x] Generate runtime tests for persistent collectives.
      Cover persistent collective initialization, start, completion,
      and request-free paths as separate executables from the blocking
      and nonblocking collective probes.
- [x] Generate runtime tests for blocking intracommunicator collectives.
- [x] Generate runtime tests for nonblocking intracommunicator
      collectives.
- [x] Generate runtime tests for reduce-scatter, scan, and exscan
      collective families.
- [x] Generate runtime tests for neighborhood collectives after topology
      communicator probes exist.
- [x] Generate runtime tests for datatype creation and introspection.
      Cover datatype constructors, commit/free, duplication, naming,
      extent/true-extent, envelope/contents, size/count helpers, and
      pack/unpack paths.  Defer datatype attribute callback behavior to
      Phase 10.
- [x] Generate runtime tests for basic datatype constructors and
      commit/free lifecycle APIs.
- [x] Generate runtime tests for indexed, hindexed, struct, subarray,
      darray, resized, and duplicate datatype constructors.
- [x] Generate runtime tests for datatype introspection, naming, extent,
      size, match-size, and F90 datatype constructor APIs.
      Installed runtime probes cover naming, extents, size, match-size,
      F90 constructors, envelope introspection, and a focused
      `MPI_Type_get_contents` probe.
- [x] Generate runtime tests for pack, unpack, external pack/unpack, and
      pack-size APIs.
- [x] Generate runtime tests for status count, element-count, source,
      tag, error, and C/status conversion APIs.
- [x] Generate runtime tests for reductions and user-defined operations.
      Cover predefined reduction operators in collective operations and
      non-callback operation-handle APIs in Phase 9.  Defer
      user-defined reduction callbacks to Phase 10.
- [x] Generate runtime tests for predefined reduction operations.
- [x] Generate runtime tests for predefined `MPI_Op` handle APIs.
      Installed runtime probes cover predefined operation commutativity,
      ABI integer conversion, and local reduction behavior.  `MPI_Op_free`
      remains part of Phase 10 user-defined operation callback testing
      because predefined operations cannot be freed.
- [x] Explicitly defer user-defined operation callback behavior to
      Phase 10 callback tests.
- [x] Generate runtime tests for attributes and keyvals.
      Phase 9 covers predefined communicator attribute lookup.  Attribute
      set/delete/free-keyval paths require user-created keyvals and
      copy/delete callbacks, so they are explicitly deferred to Phase 10
      callback tests.
- [x] Generate runtime tests for predefined communicator attribute
      keyvals.
- [x] Generate runtime tests for non-callback predefined attribute get
      paths.
- [x] Explicitly defer keyval creation and attribute copy/delete
      callbacks to Phase 10 callback tests.
- [x] Generate runtime tests for error handlers.
      Cover predefined error-handler get/set/call/free behavior and
      dynamic error class/code/string APIs.  Defer user callback
      creation behavior to Phase 10.
- [x] Generate runtime tests for dynamic error class, code, string, and
      removal APIs.
- [x] Generate runtime tests for predefined error-handler get/set/call
      paths on communicator, file, session, and window objects where the
      corresponding object family is enabled.
- [x] Explicitly defer error-handler creation callback behavior to
      Phase 10 callback tests.
- [x] Generate runtime tests for generalized requests.
      Generalized request start requires callback functions, so full
      behavior belongs in Phase 10.  Phase 9 should either cover only
      non-callback request completion/lifecycle pieces reachable without
      user callbacks or explicitly defer the family.
      The implemented generalized request APIs require a request created
      by `MPI_Grequest_start`, so the family is explicitly deferred to
      Phase 10 callback tests.
- [x] Explicitly defer generalized request callback behavior to
      Phase 10 callback tests.
- [x] Generate runtime tests for RMA/window APIs.
      Cover window creation/allocation, info/name/attribute-free
      metadata, synchronization, one-sided data movement, atomics, and
      teardown.  Gate the family on configured RMA support.
- [x] Generate runtime tests for window create, allocate,
      allocate-shared, create-dynamic, attach, detach, and free APIs.
- [x] Generate runtime tests for window info, name, group, flavor, and
      model APIs.
      Window integer-handle conversion remains covered by the Phase 8
      converter probes rather than this runtime object-state probe.
- [x] Generate runtime tests for active- and passive-target RMA
      synchronization APIs.
- [x] Generate runtime tests for RMA put/get/accumulate, get-accumulate,
      compare-and-swap, fetch-and-op, and request-based variants.
- [x] Generate runtime tests for memory allocation, address arithmetic,
      and address-difference helper APIs.
- [x] Generate runtime tests for MPI-IO APIs.
      Cover file open/close/delete, view/info/size/position/atomicity
      state, blocking/nonblocking/split-collective data movement, and
      file error handlers.  Gate the family on configured MPI-IO
      support.  Defer datarep callbacks to Phase 10.
- [x] Generate runtime tests for MPI-IO file lifecycle, info, view,
      size, position, and atomicity APIs.
- [x] Generate runtime tests for blocking MPI-IO read/write APIs.
- [x] Generate runtime tests for nonblocking and split-collective
      MPI-IO read/write APIs.
- [x] Generate runtime tests for shared-file-pointer and ordered MPI-IO
      APIs.
- [x] Explicitly defer datarep callback behavior to Phase 10 callback
      tests where Open MPI support exists.
- [x] Generate runtime tests for dynamic process management APIs.
      Cover port open/close, name publish/lookup/unpublish, spawn,
      spawn_multiple, parent-communicator lookup, connect/accept, and
      disconnect.  Keep these in separate executables because launcher
      and process-tree failures are likely to poison the MPI job.
      `MPI_Comm_join` is explicitly skipped with a stable reason because
      it needs a connected file descriptor rather than an ordinary MPI
      launcher setup.
- [x] Generate runtime tests for port and name-service APIs.
- [x] Generate runtime tests for spawn, spawn_multiple, get-parent, and
      disconnect APIs.
- [x] Generate runtime tests for connect, accept, and disconnect APIs.
- [x] Close current standard ABI C runtime coverage for MPI_T APIs.
      Cover MPI_T initialization/finalization and non-callback control,
      performance, category, enum, and handle APIs where Open MPI
      implements standard ABI support.  Defer MPI_T callback/event
      behavior to Phase 10 where applicable.
      The current installed metadata/header audit does not expose
      implemented standard ABI C MPI_T APIs.  Future MPI_T ABI support
      will be reported by the Phase 9 runtime audit.
- [x] Close current MPI_T control-variable discovery and handle API
      coverage.
- [x] Close current MPI_T performance-variable discovery, session,
      handle, read/write, and reset API coverage.
- [x] Close current MPI_T category and enum introspection API coverage.
- [x] Explicitly defer MPI_T event callback behavior to Phase 10
      callback tests where Open MPI support exists.
- [x] Generate runtime tests for big-count variants.
      Cover `_x`, `_c`, and other large-count variants after the base
      family probe exists, using small runtime counts where possible so
      the test validates ABI signature/value handling without requiring
      huge memory.
- [x] Close current big-count point-to-point and collective variant
      coverage.
      The current installed metadata/header audit does not expose
      additional implemented standard ABI C big-count point-to-point or
      collective APIs beyond the covered runtime families.
- [x] Generate runtime tests for big-count datatype, status, and
      element-count variants.
      Installed runtime probes cover the implemented `_x` datatype and
      status count helpers currently declared by the standard ABI header.
- [x] Close current big-count RMA and MPI-IO variant coverage.
      The current installed metadata/header audit does not expose
      additional implemented standard ABI C big-count RMA or MPI-IO APIs
      beyond the covered runtime families.
- [x] Generate runtime tests for miscellaneous utility APIs.
      Cover timing, memory allocation/free, address arithmetic, dims,
      name-service helpers not already covered by dynamic-process
      probes, and any other implemented non-callback APIs that do not
      fit the larger runtime families.
- [x] Explicitly skip APIs that cannot be tested without intentionally
      terminating the MPI job, such as `MPI_Abort`, unless a separate
      isolated negative-test strategy is added.
- [x] Ensure all Phase 9 output handles, statuses, errors, and
      observable side effects are checked for standard ABI values.
      A checked Phase 9 task should validate returned handles, statuses,
      counts, errors, and observable side effects.  A probe that only
      checks `MPI_SUCCESS` is not enough for family completion unless
      the API has no observable output.
      Callback argument checks are deferred to Phase 10 callback tests.

## Phase 10: Callback and Lifetime Tests

- [x] Add a Phase 10 callback coverage audit.
      Report every implemented metadata API whose entry or parameters
      require callbacks, and require each one to be covered by a
      callback probe, explicitly skipped because Open MPI lacks support,
      or deferred with a stable machine-readable reason.  The audit must
      catch callback APIs that Phase 9 deliberately did not cover.
- [x] Keep Phase 10 callback probes isolated by executable.
      Each callback behavior probe should run in its own executable, as
      in Phase 9, because a failing callback can leave the MPI object,
      request, or process state undefined.
- [x] Test communicator attribute copy/delete callback wrapping.
      Cover communicator keyval creation, attribute put/get/delete,
      duplicate-triggered copy callbacks, delete callbacks on explicit
      delete and communicator free, callback extra state, and returned
      ABI communicator/keyval values.
- [x] Test legacy communicator attribute callback APIs.
      Cover standard ABI behavior for implemented legacy
      `MPI_Keyval_create`, `MPI_Keyval_free`, `MPI_Attr_put`,
      `MPI_Attr_get`, and `MPI_Attr_delete` paths, or explicitly mark
      them outside the standard ABI when metadata/header authority says
      they are not in scope.
      The installed standard ABI C header comments out these legacy
      entry points, so the callback audit records them as not declared
      rather than generating runtime probes for non-ABI APIs.
- [x] Test datatype attribute copy/delete callback wrapping.
      Cover datatype keyval creation, attribute put/get/delete,
      duplicate-triggered copy callbacks, delete callbacks on explicit
      delete and datatype free, callback extra state, and returned ABI
      datatype/keyval values.
- [x] Test window attribute copy/delete callback wrapping.
      Cover window keyval creation, attribute put/get/delete, delete
      callbacks on explicit delete and window free, callback extra
      state, and returned ABI window/keyval values.  Do not claim a
      duplicate-copy window path, because MPI has no `MPI_Win_dup`.
- [x] Test communicator error handler callback ABI values.
      Cover create/set/get/call/free behavior, callback invocation,
      communicator handle conversion, error-code value conversion, and
      replacement/restoration of the predefined error handler.
- [x] Test file error handler callback ABI values.
      Cover file create/set/get/call/free behavior where MPI-IO is
      configured, callback invocation, file handle conversion, and
      error-code value conversion.
- [x] Test session error handler callback ABI values.
      Cover session create/set/get/call/free behavior where sessions
      are configured, callback invocation, session handle conversion,
      and error-code value conversion.
- [x] Test window error handler callback ABI values.
      Cover window create/set/get/call/free behavior where RMA is
      configured, callback invocation, window handle conversion, and
      error-code value conversion.
- [x] Test user-defined reduction callback datatype conversion.
      Cover `MPI_Op_create`, collective invocation with a real callback,
      datatype argument conversion, input/output buffer semantics,
      commutativity flag behavior, and `MPI_Op_free` cleanup.
- [x] Test generalized request callback behavior.
      Cover `MPI_Grequest_start`, `MPI_Grequest_complete`,
      wait/test-triggered query callbacks, free callbacks, cancel
      callbacks, callback extra state, and request cleanup semantics.
- [x] Test datarep callback behavior where Open MPI support exists.
      Cover `MPI_Register_datarep` read, write, and extent callbacks
      with MPI-IO probes when the configured build supports the
      functionality; otherwise skip with a stable reason.
- [x] Test MPI_T event callback behavior where Open MPI support exists.
      Cover MPI_T callback registration, callback invocation,
      callback-provided object values, and cleanup where Open MPI
      implements standard ABI MPI_T event support; otherwise skip with
      a stable reason.
- [x] Test nonblocking and persistent operations that retain converted
      arrays until completion or request release.
      Cover APIs whose converted arrays must remain alive after the
      initial wrapper call returns.  Use the nonblocking and persistent
      `MPI_Alltoallw` forms as the representative cases because their
      standard ABI wrappers must convert count, displacement, and
      datatype arrays and those arrays must remain valid through
      completion or persistent request release.
      `MPI_Ialltoallw` covers the nonblocking collective completion
      boundary.  `MPI_Alltoallw_init` covers persistent collective
      request lifetime through repeated `MPI_Start`/`MPI_Wait` cycles and
      `MPI_Request_free`.  Phase 9 runtime probes cover the broader
      request, partitioned, RMA, and MPI-IO API families for ordinary ABI
      invocation and output semantics; this Phase 10 item is specifically
      about ABI-owned retained conversion state.
- [x] Test cleanup paths for callback wrappers and retained converted
      arrays.
      Exercise cleanup paths that are observable from portable MPI
      programs: success paths, explicit free paths, cancellation paths
      where meaningful, and object-free paths.  Do not force arbitrary
      MPI operation failures after which the MPI runtime state is
      undefined; those branches are not useful functional ABI tests.
      Attribute, errhandler, generalized-request, datarep, and MPI_T
      probes validate callback cleanup through object free, explicit
      free, request free, or unregister paths as applicable.  Retained
      array probes release requests and heap storage through common
      cleanup paths after success and local failure branches.
- [x] Verify callback probes check observable side effects.
      A callback probe must validate that the callback actually ran,
      that it saw standard ABI argument values, that output arguments
      were consumed by MPI, and that cleanup happened exactly once.
      The callback probes check invocation counters, callback argument
      values, copied/deleted attributes, callback-produced data, and
      cleanup counters.  The retained-array probes check the observable
      data movement after caller-side ABI arrays have been overwritten,
      which verifies that MPI did not depend on stale caller storage.

## Phase 11: Fortran Regression and ABI Tests

- [x] Detect configured `mpif.h` support.
      Use configure output and installed wrapper behavior so disabled
      bindings skip with stable reasons rather than failing or silently
      disappearing.
- [x] Detect configured `use mpi` support.
      Use configure output and installed wrapper behavior so disabled
      bindings skip with stable reasons rather than failing or silently
      disappearing.
- [x] Detect configured `use mpi_f08` support.
      Use configure output and installed wrapper behavior so disabled
      bindings skip with stable reasons rather than failing or silently
      disappearing.
- [x] Add a Phase 11 Fortran coverage audit.
      Report every metadata API expressible in each configured Fortran
      binding layer, grouped by `mpif.h`, `use mpi`, and `use mpi_f08`,
      with configured binding state, installed wrapper availability,
      implemented counts, seed compile-probe coverage, and pending
      Phase 11B counts.
      Chunk 11A adds the grouped report as an advisory audit with
      `pending_phase11b` counts.  It deliberately does not hard-fail on
      missing exhaustive Fortran coverage until the Chunk 11B generated
      compile/runtime probes exist.
- [x] Keep Phase 11 Fortran runtime probes isolated by executable.
      Generate one executable per logical Fortran test case so a failed
      MPI job or failed Fortran binding invocation does not affect later
      Fortran probes.
- [ ] Generate exhaustive `mpif.h` regression tests for APIs supported
      by the configured build.
      These are regression tests for Open MPI's existing binding, not
      standard Fortran ABI tests.  Generate compile and runtime probes
      for configured, metadata-supported `mpif.h` APIs and skip entries
      unavailable in the configured compiler/build.
- [ ] Generate exhaustive `use mpi` regression tests for APIs supported
      by the configured build.
      These are regression tests for Open MPI's existing binding, not
      standard Fortran ABI tests.  Generate compile and runtime probes
      for configured, metadata-supported `use mpi` APIs and skip entries
      unavailable in the configured compiler/build.
- [ ] Generate exhaustive `use mpi_f08` regression tests for APIs
      supported by the configured build.
      Generate compile and runtime probes for configured,
      metadata-supported `use mpi_f08` APIs.  This layer also owns
      standard Fortran ABI functionality that Open MPI implements.
- [x] Add initial Fortran compile-only interface conformance probes.
      Verify subroutine/function names, argument ranks, optional
      arguments, kind parameters, derived types, and overload resolution
      for each configured Fortran binding without launching MPI where a
      compile-time check is sufficient.
      Chunk 11A adds one installed compile-only probe per configured
      binding layer.  The probes compile and link through `mpifort` but
      do not launch MPI.  Exhaustive per-API compile conformance remains
      open under the binding-specific generation tasks above.
- [ ] Generate Fortran runtime probes for initialization, communicators,
      groups, datatypes, requests, statuses, collectives, and MPI-IO
      where the configured binding supports them.
      Use the Phase 9 C runtime families as the coverage model, but
      keep each Fortran binding's probes separately classified so a
      missing `mpif.h` path cannot be hidden by `use mpi_f08` coverage.
      Chunk 11B adds core runtime probes for initialization,
      communicators, point-to-point status handling, datatypes, and
      barriers in each binding layer.  Broader generated runtime
      families for groups, requests, collectives beyond barrier, MPI-IO,
      and the rest of the Phase 9 model remain open here.
- [x] Test Fortran `MPI_Abi_get_version`.
- [x] Test Fortran `MPI_Abi_get_info`.
- [x] Test Fortran `MPI_Abi_get_fortran_info`.
- [x] Test Fortran `MPI_Abi_set_fortran_info`.
- [x] Test Fortran `MPI_Abi_get_fortran_booleans`.
- [x] Test Fortran `MPI_Abi_set_fortran_booleans`.
- [ ] Test `use mpi_f08` ABI functionality implemented by Open MPI.
      Cover implemented `use mpi_f08` ABI entry points and helpers with
      compile and runtime probes.  Unimplemented standard Fortran ABI
      functionality must be skipped explicitly rather than omitted.
- [ ] Test Fortran standard ABI handle and status conversions.
      Verify that `use mpi_f08` handles, statuses, ignore sentinels,
      status arrays, and optional output arguments agree with the
      standard ABI behavior Open MPI implements.
      Chunk 11B validates `use mpi_f08` typed communicator duplication
      and free, typed datatype use through `MPI_Type_size`, typed status
      values through `MPI_Sendrecv`/`MPI_Get_count`, and optional
      `ierror` omission on a compile-only call.  Broader ignore-sentinel
      and status-array coverage remains part of the open exhaustive
      `use mpi_f08` ABI task above.
- [ ] Explicitly skip unimplemented standard Fortran ABI functionality
      with stable machine-readable reasons.
- [x] Test Fortran logical size and true/false value reporting.
- [ ] Test optional Fortran datatype size and unavailable-type behavior.
      Optional Fortran datatype probes must be driven by the configured
      compiler/build and the installed standard ABI header, so an
      unavailable optional type is a documented skip and an incorrectly
      declared available type is a hard failure.
- [x] Verify Fortran reports distinguish regression coverage from
      standard ABI coverage.
      Reports must separately show `mpif.h`, `use mpi`, `use mpi_f08`
      regression coverage and implemented `use mpi_f08` ABI coverage so
      a pass in one category cannot mask missing coverage in another.

## Phase 12: MPICH Cross-Implementation Tests

- [ ] Add optional `make check-abi-cross` runner mode.
      This target should be separate from `make check-abi`, skip cleanly
      when MPICH tools are unavailable, and reuse the same generated
      source/reporting infrastructure where practical.
- [ ] Discover MPICH compiler wrapper and launcher from `PATH`.
- [ ] Honor MPICH override variables.
- [ ] Honor library path and launcher environment overrides for Open MPI
      and MPICH.
- [ ] Select Linux or macOS runtime loader variables as appropriate.
- [ ] Detect unsupported cross-test platforms or loader configurations
      with stable skip reasons.
      Missing runtime-loader controls, unsupported library naming, or
      unavailable inspection tools should not be reported as ABI
      failures unless a compile/run test proves an ABI mismatch.
- [ ] Build a cross-test environment report.
      Record Open MPI and MPICH compiler wrappers, launchers, versions,
      include paths, link paths, loader environment, temporary paths,
      and selected cross-test direction in the JSON and text reports.
- [ ] Compile with MPICH and run with Open MPI's `libmpi_abi`.
      Compile standard ABI source with MPICH's wrapper, force the
      runtime library path to Open MPI's installed `libmpi_abi`, launch
      with the selected launcher, and verify that handles, constants,
      and runtime results match standard ABI expectations.
- [ ] Compile with Open MPI's `mpicc_abi` and run with MPICH.
      Compile standard ABI source with Open MPI's ABI wrapper, force the
      runtime library path to MPICH's standard ABI library, launch with
      the selected launcher, and verify that handles, constants, and
      runtime results match standard ABI expectations.
- [ ] Reuse Phase 8 converter probes in both cross directions.
      Cross-run the converter probes before larger runtime probes so
      handle and constant mapping mismatches are detected with small,
      easy-to-triage executables.
- [ ] Reuse Phase 9 runtime API probes in both cross directions.
      Cross-run the C runtime API family probes that are portable across
      both implementations, with explicit skips for features disabled or
      not implemented by either side.
- [ ] Reuse Phase 10 callback probes in both cross directions where
      both implementations support the callback functionality.
- [ ] Semantically compare Open MPI and MPICH standard ABI headers when
      MPICH mode is enabled.
      Compare constants, typedefs, prototypes, ABI helper declarations,
      and feature availability using parsed header data, not substring
      checks.  Differences from MPI standard ABI authority are hard
      failures; unavailable optional features are stable skips.
- [ ] Compare Open MPI and MPICH wrapper compile/link intent.
      Verify that each wrapper exposes the expected standard ABI include
      path and ABI library link intent without treating a substring in
      an unrelated path as sufficient evidence.
- [ ] Add optional cross-direction linkage diagnostics.
      Use platform tools when available to show which ABI library the
      generated executable will load; missing diagnostic tools should be
      skips, while a proven wrong linked library is a failure.
- [ ] Report MPICH tool paths and versions.
- [ ] Skip cross tests with stable machine-readable reasons when MPICH
      tools are unavailable.
- [ ] Collect cross-test failures and emit a combined summary.
      Do not stop at the first failed direction or first failed probe;
      record per-direction PASS/FAIL/SKIP status and emit aggregate
      counts suitable for CI tracking.

## Phase 13: Completion Gate

- [ ] Enable hard failure for unclassified metadata entries.
- [ ] Enable hard failure for `test_not_written_yet` on implemented ABI
      functionality.
- [ ] Gate completion on both API and constant test status.
      The complete-suite gate must include implemented APIs and ABI
      constants so unwritten constant tests cannot be hidden by complete
      API coverage.
- [ ] Verify every API and ABI constant from the authority metadata is
      classified.
- [ ] Verify all implemented C standard ABI functionality is tested.
      Require every implemented C API to be covered by compile/link,
      symbol, converter, runtime, callback, or explicit skip/defer
      logic appropriate to that API's metadata and family.
- [ ] Verify all configured Fortran binding layers have exhaustive
      regression coverage.
- [ ] Verify implemented `use mpi_f08` ABI functionality is tested.
- [ ] Verify unimplemented standard ABI functionality is explicitly
      skipped.
- [ ] Verify callback-heavy and resource-sensitive APIs are not omitted.
      Completion must account for dynamic process management,
      attribute/error-handler callbacks, generalized requests,
      user-defined operations, datarep callbacks, MPI_T callbacks, RMA,
      MPI-IO, and any APIs intentionally skipped as untestable in local
      CI.
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
- [ ] Verify distribution tarballs include ABI specs, templates,
      scripts, requirements, and runner inputs.
      `make dist` and `make distcheck` must include the source files
      needed to regenerate ABI test probes without checking generated
      probe sources into git.
- [ ] Verify VPATH builds run ABI tests from source and distribution
      trees.
      Run representative `check-fast`, `check-abi`, and optional
      `check-abi-cross` targets from an out-of-tree build so source-tree
      path assumptions do not pass only in non-VPATH builds.
- [ ] Verify JSON and text reports are emitted and suitable for CI
      tracking.
- [ ] Verify complete-gate behavior for PASS, FAIL, and SKIP runs.
      A legitimate skip, such as standard ABI support disabled or
      missing optional cross tools, must not be converted into a
      completion-gate failure; missing implemented coverage must remain
      a hard failure when the suite is complete.
- [ ] Verify command timeouts and failure summaries cover launcher
      hangs.
      Runtime probes that call `mpirun` must have configurable timeouts
      and record timeout failures in the same JSON/text summary as other
      runtime failures.
