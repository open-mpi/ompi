.. _release-notes-mpi-5.0-ch11:

Process Initialization, Creation, and Management
================================================


.. admonition:: tl;dr
   :class: tip

   Open MPI on main (HEAD 71d6dcf7f4) provides a default, conforming implementation of essentially all of Chapter 11: the World Model (Init/Init_thread/Finalize, four thread levels incl. MPI_THREAD_MULTIPLE, MPI_INFO_ENV, Initialized/Finalized, Abort), the Sessions Model (Session_init/finalize, pset enumeration/query, Comm/Intercomm_create_from_group), and full dynamic process management (Comm_spawn[_multiple], Comm_get_parent, Open/Close_port, Comm_accept/connect/disconnect, Comm_join, Publish/Lookup/Unpublish_name). The central reconciliation question is b3's labeling of all 13 dynamic/session-pset routines as 'optional_lib_gated': per the framework's pre-resolved fact (and b3's OWN caveat at ch11.json:652-655 stating PMIx is a MANDATORY build dependency so every routine compiles unconditionally), these are SUPPORTED by default with a runtime gate note 'requires PRRTE/PMIx runtime (default, ships with Open MPI)', NOT CONDITIONAL. The b3 status is a labeling difference about RUNTIME capability, not a real conformance conflict. The only genuine non-default-path item is the FT-MPI/ULFM subset-abort resilience advice-to-implementors behavior; the only genuine gap is the two Table-11.1 ABI routines (MISSING_INFLIGHT via PR #13280). 'soft' spawning and other uninterpreted reserved keys are expressly permitted by the standard (NA). The mpi_memory_alloc_kinds round-trip on MPI_File is in-flight (#13367).

Conformance summary
-------------------


**33** reconciled requirement(s)/behavior(s): 28 supported, 0 conditional, 1 partial, 0 unsupported, 2 missing-but-in-flight, 2 N/A (90.3% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


mpi_memory_alloc_kinds round-trip on MPI_File (MPI_FILE_GET_INFO returns the kinds)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §11.4.3 / p.504
:Reviewers: b1=silent, b2=MISSING_INFLIGHT, b3=silent (raised by 1 reviewer)
:Evidence: ``framework in-flight tiebreaker #2 (OMPIO info round-trip, issue #13367 / branch pr/ompio-and-mpi-info-lets-be-friends)``
:Notes: Unique to b2 and matches the framework's in-flight fact: the full File info round-trip (so memory-alloc-kinds appears in MPI_FILE_GET_INFO) ties into OMPIO info round-tripping, in-flight via #13367. Session/comm/win paths are SUPPORTED (separate topic above).

Table 11.1 ABI routines MPI_Abi_get_version / MPI_Abi_get_info callable before init
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §11.4.1 / p.503
:Reviewers: b1=PARTIAL, b2=silent, b3=silent (raised by 1 reviewer)
:Evidence: ``ompi/include/mpi.h.in (grep MPI_Abi_get_version / MPI_Abi_get_info -> absent); framework in-flight tiebreaker (PR #13280)``
:Notes: The two ABI Table-11.1 routines are absent on main. Per the framework's authoritative in-flight tiebreaker (all MPI_Abi\_\* surface in-flight via PR #13280), this is MISSING_INFLIGHT, not PARTIAL/UNSUPPORTED. b1's '#13280' note already identified the in-flight source.

MPI_Session_get_info returns the session's hints incl. the provided thread_level
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §11.3.1, §11.3.4 / p.490
:Reviewers: b1=PARTIAL, b2=SUPPORTED, b3=silent (reviewers disagreed)
:Evidence: ompi/instance/instance.c:902 (ONLY mpi_memory_alloc_kinds subscribed into session info via opal_infosubscribe_subscribe); ompi/mpi/c/session_get_info.c.in (dups session->super.s_info into info_used — no thread_level added); session_init.c.in:45 (thread_level parsed, stored as int i_thread_level instance.h:31, never re-published as a key); info.c:195-204 sets 'thread_level' only on the WORLD-model MPI_INFO_ENV object, NOT the session info
:Notes: Conflict resolved to PARTIAL (b1 correct, b2 wrong). The MPI-5.0 standard text is explicit and normative: §11.3.1 (p.490, PDF) states 'The thread support level actually provided by the MPI implementation can be determined via a subsequent call to MPI_SESSION_GET_INFO to return the info object associated with the Session', and §11.3.4 requires GET_INFO to return 'all hints that are supported by the implementation and have default values specified; any user-supplied hints that were not ignored by the implementation'. thread_level is a supported, non-ignored predefined hint (parsed and acted on at session_init.c.in:45), yet Open MPI never adds it to session->super.s_info — only mpi_memory_alloc_kinds is subscribed (instance.c:902). So MPI_Session_get_info cannot report the provided thread level as the standard designates. Present but not fully conformant -> PARTIAL. (The thread_level key IS set on the separate MPI_INFO_ENV object at info.c:195-204, which does not satisfy this requirement.)

Subset-abort resilience: non-aborted processes keep communicating and receive MPI_ERR_PROC_ABORTED when touching aborted peers
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **N/A**
:Standard: §11.4.2 / p.504 (advice to implementors)
:Reviewers: b1=silent, b2=CONDITIONAL, b3=silent (raised by 1 reviewer)
:Evidence: MPI-5.0 standard §11.4.2 MPI_Abort, 'Advice to implementors' box (for-claude/mpi50-report.pdf, verified text): 'After aborting a subset of processes, a high-quality implementation should be able to provide error handling... the remaining processes ... should be able to continue communicating ... and receive an appropriate error code ... (e.g., an error of class MPI_ERR_PROC_ABORTED). ... (End of advice to implementors.)'; ompi/errhandler/errhandler.c:416 (sole runtime delivery of MPI_ERR_PROC_ABORTED) inside #if OPAL_ENABLE_FT_MPI (block 314-452); errhandler.c:521-525 default fall-through aborts; runtime gate ompi_ftmpi_enabled default false (ompi/runtime/ompi_mpi_params.c:100-126)
:Notes: Resolved to NA, now grounded in the verified standard text. The MPI-5.0 §11.4.2 surviving-process continued-communication / MPI_ERR_PROC_ABORTED-delivery behavior is explicitly enclosed in an 'Advice to implementors' box ('a high-quality implementation should ...', ending '(End of advice to implementors.)') — non-binding, not a normative requirement. Per the taxonomy, advice-to-implementors that does not bind an implementation = NA. b2 (with detailed verification) is correct on the SOURCE facts (delivery exists only under --with-ft=mpi / OPAL_ENABLE_FT_MPI plus runtime ompi_ftmpi_enabled, not in a default build), so its CONDITIONAL would be right IF the behavior were normative; but because the standard frames it as advice, NA is the correct taxonomy value. The genuinely required behavior — MPI_Abort aborting the comm group, plus the existence of MPI_ERRORS_ABORT / MPI_ERR_PROC_ABORTED — is SUPPORTED (separate topics above).

'soft' spawning: returning a partial process count instead of MPI_ERR_SPAWN
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **N/A**
:Standard: §11.8.4 / p.524
:Reviewers: b1=UNSUPPORTED, b2=PARTIAL, b3=silent (reviewers disagreed)
:Evidence: ``ompi/dpm/dpm.c:1036 ('soft -- to be implemented'); ompi/dpm/dpm.c:863 (comment referencing 'soft' MPI-2 semantics)``
:Notes: Conflict resolved to NA. Both b1 (UNSUPPORTED) and b2 (PARTIAL) are wrong on the taxonomy: the 'soft' reserved key is one the implementation NEED NOT interpret (§11.8.4), and the standard explicitly does NOT require soft spawning. An optional feature MPI permits omitting = NA. Open MPI does not interpret 'soft' (dpm.c:1036), which is conformant. b1's own caveat already acknowledges 'standard explicitly does not require soft spawning'.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - MPI_Init / MPI_Init_thread bootstrap the World Model; accept NULL argc/argv; exactly-once; COMM_WORLD/SELF valid after
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Four thread levels granted incl. MPI_THREAD_MULTIPLE; provided>=requested; thread-compliant operation
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Query_thread / MPI_Is_thread_main report negotiated provided level / identify main thread
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Initialized / MPI_Finalized always callable before init and after finalize; thread-safe (Table 11.1 always-available subset)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Finalize cleans up World-Model state; collective over connected procs; COMM_SELF freed first triggering attribute delete callbacks in reverse insertion order
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - mpi_initial_errhandler info key selects initial error handler (errors_are_fatal / errors_abort / errors_return) before/during init
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - MPI_INFO_ENV exposes predefined keys (command, argv, maxprocs, mpi_initial_errhandler, soft, host, arch, wdir, thread_level)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Session_init local & thread-safe; honors thread_level and mpi_memory_alloc_kinds info keys; multiple sessions per process
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Session_finalize cleans up session state, sets handle to MPI_SESSION_NULL, may synchronize on derived groups
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Mandated process sets mpi://WORLD and mpi://SELF always available; pset index/name stability; additional runtime psets enumerable
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Session_get_num_psets / get_nth_pset / get_pset_info: enumeration, truncation/length (two-call idiom), mandatory mpi_size key
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - MPI_Group_from_session_pset / MPI_Comm_create_from_group / MPI_Intercomm_create_from_groups build group/comm/intercomm from psets/groups
     - b1=SUPP/b2=sile/b3=COND (conflict)
   * - mpi_memory_alloc_kinds (default 'mpi,system') request/query and mpi_assert_memory_alloc_kinds across session/comm/win
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Abort best-effort aborts the group of comm / connected processes; default aborts MPI_COMM_WORLD; returns errorcode to environment
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_ERRORS_ABORT predefined initial error handler and MPI_ERR_PROC_ABORTED error class exist
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Comm_spawn / MPI_Comm_spawn_multiple start maxprocs children, return intercomm, MPI_ERR_SPAWN on failure; MPI_ARGV(S)_NULL, MPI_ERRCODES_IGNORE honored; ordered child ranks
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - Reserved spawn info keys (host, wdir, path, arch, file, soft) interpreted per §11.8.4 if the implementation chooses to interpret them
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - MPI_Comm_get_parent returns parent intercomm in spawned children, MPI_COMM_NULL otherwise
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Open_port / MPI_Close_port establish/release a system port_name (<= MPI_MAX_PORT_NAME)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Comm_accept / MPI_Comm_connect establish intercomm via port; MPI_ERR_PORT on bad/timed-out port
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - MPI_Publish_name / MPI_Unpublish_name / MPI_Lookup_name with MPI_ERR_SERVICE / MPI_ERR_NAME; info-controlled scope
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - MPI_Comm_join bootstraps an intercommunicator from a connected socket fd
     - b1=COND/b2=SUPP/b3=COND (conflict)
   * - MPI_Comm_disconnect waits for pending comm, frees comm, sets MPI_COMM_NULL, decrements connection refcount; rejects COMM_WORLD/COMM_SELF
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Singleton MPI initialization: a process started without mpiexec becomes size-1 COMM_WORLD and can later spawn
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_UNIVERSE_SIZE predefined attribute on MPI_COMM_WORLD set at init when provided by startup
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_APPNUM predefined attribute on MPI_COMM_WORLD reflects spawn/multi-app command number
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Table 11.1 functions callable before init / after finalize (non-ABI routines)
     - b1=PART/b2=sile/b3=sile (unique)
   * - Thread-safety: Table 11.1 subset always thread-safe; blocking calls block only the calling thread
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
