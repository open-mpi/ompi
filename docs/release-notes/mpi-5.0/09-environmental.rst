.. _release-notes-mpi-5.0-ch09:

MPI Environmental Management
============================


.. admonition:: tl;dr
   :class: tip

   Chapter 9 is broadly implemented and conformant in Open MPI HEAD (71d6dcf7f4) by default, in C and use-mpi-f08: init/init_thread/finalize/initialized/finalized, the Sessions model (session_init/finalize/get_info/get_num_psets/get_nth_pset/get_pset_info, comm_create_from_group), MPI_Abort, the full error-handling surface (create/set/get/call/free errhandler for comm/win/file/session, all standard error classes/codes/strings, Add/Remove_error\_\*), Get_processor_name, Get_version/library_version, Wtime/Wtick, Aint_add/diff (macros), Alloc_mem/Free_mem (mpi_minimum_memory_alignment + MPI_ERR_NO_MEM), and the predefined attributes (TAG_UB, IO, WTIME_IS_GLOBAL, LASTUSEDCODE). Three genuine quality gaps remain, all on default builds with no gate: (1) the tree advertises MPI 4.1 (MPI_VERSION=4/MPI_SUBVERSION=1, Get_version returns (4,1)), not the 5.0 being audited (PARTIAL value gap); (2) MPI_Get_hw_resource_info is a stub returning an empty MPI_Info with no hwloc://-style resource keys, so the HW/RESOURCE_GUIDED discovery workflow finds nothing (PARTIAL); (3) MPI_ERRORS_ABORT is fully wired for comm/win/file but the session/instance variant is missing (eh_instance_fn left NULL) and the invoke path has no NULL guard, so ERRORS_ABORT on a session NULL-derefs/crashes instead of aborting the local process (PARTIAL). MPI_ERR_ABI is the only absent standard error class and is in-flight (PR #13280, ABI chapter).

Conformance summary
-------------------


**30** reconciled requirement(s)/behavior(s): 25 supported, 0 conditional, 3 partial, 0 unsupported, 2 missing-but-in-flight, 0 N/A (83.3% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI_ERR_ABI error class (referenced by the ABI feature)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: MPI-5.0 Ch.20 (ABI), referenced from §9.4
:Reviewers: b1=silent, b2=MISSING_INFLIGHT, b3=silent (raised by 1 reviewer)
:Gate: PR #13280 (ABI chapter)
:Evidence: ``grep MPI_ERR_ABI ompi/include/mpi.h.in -> absent; for-claude/work/inflight.json (PR #13280)``
:Notes: Only missing standard error class; added by the in-flight ABI PR per the framework's in-flight tiebreaker -> MISSING_INFLIGHT, not UNSUPPORTED.

MPI_File info round-trip: MPI_File_get_info returns hints set via open/set_info/set_view (file objects carry info+errhandler)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §9.3.3 (file errhandler/info), §14 (I/O)
:Reviewers: b1=silent, b2=MISSING_INFLIGHT, b3=silent (raised by 1 reviewer)
:Gate: ompio-info branch pr/ompio-and-mpi-info-lets-be-friends (issue #13367)
:Evidence: ``for-claude/work/inflight.json (ompio_mpi_info); ompi/mca/io/ompio, ompi/mca/common/ompio; the MPI_File_get_info/set_info C+f08 symbols are present already``
:Notes: b2 raised this; carried per the framework's in-flight tiebreaker, which names this exact item. Peripheral to Chapter 9 (mostly Chapter 14), noted because file objects carry info+errhandler. The MPI_File_get_info/set_info symbols exist; only the full hint round-trip BEHAVIOR is in-flight -> MISSING_INFLIGHT, not UNSUPPORTED.

MPI_Get_version returns (version,subversion); thread-safe; callable before init / after finalize
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §9.1.1 / p.438
:Reviewers: b1=PARTIAL, b2=UNSUPPORTED, b3=silent (reviewers disagreed)
:Evidence: ompi/mpi/c/get_version.c.in:66-67 (\*version=MPI_VERSION; \*subversion=MPI_SUBVERSION); VERSION:23-24 (mpi_standard_version=4, mpi_standard_subversion=1); config/autogen_found_items.m4:12-13 (MPI_VERSION_NUM=4, MPI_SUBVERSION_NUM=1)
:Notes: Procedure is correct, thread-safe (reads compile-time macros, no global state), callable any time. The defect is the VALUE: the tree hardcodes MPI 4.1, so Get_version returns (4,1) and the MPI_VERSION/MPI_SUBVERSION C macros + Fortran params are 4/1, not the (5,0) MPI-5.0 §9.1.1 mandates. No configure flag/MCA/library sets version=5; not in-flight (PR #13280 only adds the distinct MPI_ABI_VERSION). b2 called this missing_gap, but the symbol is present and works — it is a value non-conformance, i.e. PARTIAL (present but not fully standard-conformant), not UNSUPPORTED.

MPI_Get_hw_resource_info returns MPI_Info of URI-form hardware-resource (key,value) pairs (e.g. hwloc://NUMANode) usable for HW/RESOURCE_GUIDED splitting
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §9.1.2 / p.441
:Reviewers: b1=PARTIAL, b2=UNSUPPORTED, b3=PARTIAL (reviewers disagreed)
:Evidence: ompi/mpi/c/get_hw_resource_info.c.in:46-64 (validates info!=NULL -> MPI_ERR_INFO; \*info=ompi_info_allocate(); MPI_ERR_NO_MEM on alloc fail; return MPI_SUCCESS with ZERO info_set calls; dev comment lines 53-56 "No resources currently being specified so just return empty info object")
:Notes: Symbol present in C and f08, validates args, returns a valid freeable empty MPI_Info — it never crashes or violates the signature, and an empty info is a defensible degenerate-but-legal 'no restrictions' return. But no hwloc://-style resource-type keys are ever populated (even though hwloc is available), so the Example 9.1 HW/RESOURCE_GUIDED discovery workflow finds nothing on every system. Present but the substantive feature is absent = PARTIAL/stub. Not UNSUPPORTED (b2): the symbol and a legal success path exist. Not CONDITIONAL: behavior is identical on every build, no gate. Neither in-flight PR touches this file.

MPI_ERRORS_ABORT (new in 4.0): comm/win/file abort the group; session aborts only the local process
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §9.3 / p.448
:Reviewers: b1=PARTIAL, b2=SUPPORTED, b3=silent (reviewers disagreed)
:Evidence: ompi/errhandler/errhandler.c:197-208 (ompi_mpi_errors_abort sets eh_comm_fn/eh_file_fn/eh_win_fn but NOT eh_instance_fn -> NULL via OBJ_CONSTRUCT zero-init); ompi/errhandler/errhandler_predefined.c:128-186 (abort comm/file/win handlers exist; NO ompi_mpi_errors_abort_instance_handler defined); ompi/errhandler/errhandler_invoke.c:129-134 (TYPE_INSTANCE case calls errhandler->eh_instance_fn with no NULL guard); ompi/mpi/c/session_set_errhandler.c.in:44-48 (accepts any PREDEFINED handler incl. ERRORS_ABORT)
:Notes: Investigated the conflict in source. comm/win/file ERRORS_ABORT are correct (backend_abort fatal=false). But the session/instance variant is genuinely broken: ompi_mpi_errors_abort.eh_instance_fn is never assigned (contrast fatal at errhandler.c:178 and return at :192 which both set it), there is no ompi_mpi_errors_abort_instance_handler function at all, and the invoke path dereferences eh_instance_fn with no NULL check. MPI_Session_set_errhandler accepts ERRORS_ABORT (it is PREDEFINED), so a subsequent session error NULL-derefs and crashes instead of aborting only the local process. b2's 'default' overlooked the missing instance wiring. PARTIAL: three of four object variants conformant, the session variant is a crash bug.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - MPI_Get_library_version returns null-terminated impl string <= MPI_MAX_LIBRARY_VERSION_STRING-1; callable any time; thread-safe
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Init / MPI_Init_thread / MPI_Finalize / MPI_Initialized / MPI_Finalized (World Model bootstrap)
     - b1=sile/b2=sile/b3=sile (unique)
   * - MPI_Abort terminates the job / processes of the communicator's group
     - b1=sile/b2=sile/b3=sile (unique)
   * - Sessions model: MPI_Session_init/finalize/get_info/get_num_psets/get_nth_pset/get_pset_info; MPI_Comm_create_from_group
     - b1=sile/b2=sile/b3=sile (unique)
   * - MPI_Get_processor_name returns a unique node specifier, null-terminated, sets resultlen <= MPI_MAX_PROCESSOR_NAME-1
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Predefined attributes MPI_TAG_UB (>=32767), MPI_IO, MPI_WTIME_IS_GLOBAL on MPI_COMM_WORLD; constant init->finalize; not user-modifiable
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Alloc_mem: aligned allocation, honors mpi_minimum_memory_alignment info key, MPI_INFO_NULL valid, MPI_ERR_NO_MEM on exhaustion
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Free_mem frees Alloc_mem memory (may raise MPI_ERR_BASE on invalid base)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Aint_add / MPI_Aint_diff
     - b1=sile/b2=sile/b3=sile (unique)
   * - Error handlers create/attach/query/free for comm/win/file/session (MPI_XXX\_{CREATE,SET,GET}_ERRHANDLER, MPI_ERRHANDLER_FREE) with type matching
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Default initial error handler is MPI_ERRORS_ARE_FATAL on predefined comms; configurable via launch key mpi_initial_errhandler
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Windows default to MPI_ERRORS_ARE_FATAL; files default to MPI_ERRORS_RETURN; neither inherits the initial handler
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_ERRORS_ARE_FATAL aborts connected processes (comm/win/file/session variants)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_ERRORS_RETURN returns the error code without aborting (comm/win/file/session)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Errhandler_free marks handler for dealloc, sets handle to MPI_ERRHANDLER_NULL
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Error_string returns a string for any code/class; callable before init / after finalize; thread-safe
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Error_class maps any code to a standard class (classes map to themselves); callable before init / after finalize
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - All standard error classes defined; 0=MPI_SUCCESS < MPI_ERR\_\* <= MPI_ERR_LASTCODE
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Add/Remove_error_class, _code, _string create/remove user classes/codes/strings; update MPI_LASTUSEDCODE; thread-safe; pre-init/post-finalize
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI\_{Comm,Win,File,Session}_call_errhandler invoke the object's handler with a user error code
     - b1=PART/b2=SUPP/b3=sile (conflict)
   * - MPI_Wtime returns elapsed wall-clock seconds; fixed time origin for process lifetime
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Wtick returns the resolution of MPI_Wtime in seconds
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Memory allocation kinds info (mpi_memory_alloc_kinds / assert) infrastructure
     - b1=sile/b2=sile/b3=sile (unique)
   * - Fortran user errhandler callback interfaces for comm/win/file/session
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - PMPI\_ profiling name-shift provided for every Chapter 9 procedure
     - b1=SUPP/b2=sile/b3=sile (unique)
