.. _release-notes-mpi-5.0-ch03:

Point-to-Point Communication
============================


.. admonition:: tl;dr
   :class: tip

   Open MPI's main branch conforms broadly to MPI-5.0 Chapter 3. All four send modes (standard/buffered/synchronous/ready), blocking and nonblocking, persistent requests, Isendrecv(_replace), wildcards (MPI_ANY_SOURCE/MPI_ANY_TAG), MPI_PROC_NULL no-op semantics, probe/matched-probe (Mprobe/Improbe/Mrecv/Imrecv + MPI_MESSAGE_NO_PROC), cancellation, multiple-completion, and the full MPI-5.0 buffer-policy overhaul (per-process/per-comm/per-session Bsend buffers with comm>session>process precedence, MPI_BUFFER_AUTOMATIC, and the new {Buffer,Comm,Session}_{attach,detach,flush,iflush}_buffer procedures) are present in C and f08 by default. Three genuine deviations were verified: (1) the MPI-4.0 status accessor procedures MPI_Status\_{get,set}_{source,tag,error} have C bindings but NO mpi_f08 binding (six procedures); (2) MPI_Get_count_c (the big-count variant) inherits the int-path INT_MAX overflow check from the shared template and wrongly returns MPI_UNDEFINED for counts above INT_MAX that fit in MPI_Count; (3) a regression on main (commit 38a7fbb837) inverts the success/error condition in MPI_Testall (only) in its non-MPI_STATUSES_IGNORE branch, so a successful MPI_Testall with a real statuses[] array returns MPI_ERR_IN_STATUS and a failed one is masked; MPI_Waitall and MPI_Testsome are unaffected. Heterogeneous representation conversion (3.3.2) is CONDITIONAL on --enable-heterogeneous (default off). Send-cancel-as-no-op and synchronous iflush are conformant QoI choices, not gaps.

Conformance summary
-------------------


**28** reconciled requirement(s)/behavior(s): 25 supported, 1 conditional, 2 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (89.3% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI_Get_count_c (big-count variant) overflow handling: must return true MPI_Count up to count-type max, not cap at INT_MAX
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §3.2.5 / p.39
:Reviewers: b1=silent, b2=silent, b3=PARTIAL (raised by 1 reviewer)
:Evidence: ompi/mpi/c/get_count_generated.c:93-131 (MPI_Get_count_c, OUT param MPI_Count\*), specifically lines 122-126: still checks 'internal_count > ((size_t) INT_MAX) -> MPI_UNDEFINED' and casts '(int)internal_count'. Inherited verbatim from the single template get_count.c.in:66-70 via the bigcount generator.
:Notes: Unique to b3; VERIFIED in generated source. For a received element count above INT_MAX (~2.1e9) that fits in MPI_Count, MPI_Get_count_c wrongly returns MPI_UNDEFINED instead of the true count, and the (int) cast truncates. The _c variant exists precisely to support such counts, so this is a real conformance deviation. Narrow edge case (>2.1G-element single receive); one-line fix (use count-type max on the _c path). Present-but-wrong, not in-flight -> PARTIAL.

Multiple completion: MPI_Testall returns spurious MPI_ERR_IN_STATUS on success (regression); Waitany/Waitall/Waitsome/Testany/Testsome correct
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §3.7.5 / p.85
:Reviewers: b1=SUPPORTED, b2=SUPPORTED, b3=PARTIAL (reviewers disagreed)
:Evidence: ompi/request/req_test.c:252 (MPI_Testall / ompi_request_default_test_all, non-MPI_STATUSES_IGNORE branch): 'if (MPI_SUCCESS == request->req_status.MPI_ERROR) { rc = MPI_ERR_IN_STATUS; }' is INVERTED -- it should be '!='. The MPI_STATUSES_IGNORE branch at req_test.c:286 is correct ('!='). VERIFIED CORRECT (unaffected): MPI_Waitall at ompi/request/req_wait.c:373 ('if (statuses[i].MPI_ERROR != OMPI_SUCCESS)') and MPI_Testsome at ompi/request/req_test.c:390 ('!='). Procedures all present (waitall.c.in, testall.c.in, etc.).
:Notes: CONFLICT (b1/b2 SUPPORTED vs b3 non_conformant). Resolved to PARTIAL. b3's regression is VERIFIED by reading source and git history: introduced by commit 38a7fbb837 ('Fix for issue #13432', 2025-10-15), which refactored both req_test.c and req_wait.c. The bug is ISOLATED to MPI_Testall (ompi_request_default_test_all): in the real-array_of_statuses branch the condition is inverted, so (a) a fully successful MPI_Testall returns MPI_ERR_IN_STATUS (firing the error handler) instead of MPI_SUCCESS, and (b) a genuinely failed request does NOT set MPI_ERR_IN_STATUS (the error is masked) and the nested FT branch is dead code. MPI_Waitall (req_wait.c:373) and MPI_Testsome (req_test.c:390) keep the correct '!=' condition and are NOT affected; the MPI_STATUSES_IGNORE path of Testall is also correct. Present-but-wrong, one-character fix ('==' -> '!='), not on the in-flight list -> PARTIAL, not MISSING_INFLIGHT. This is a correctness regression on main worth fixing promptly. Filed as open-mpi/ompi issue #13967.

Data/representation conversion across heterogeneous environments (different binary representations / endianness)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §3.3.2 / p.49
:Reviewers: b1=CONDITIONAL, b2=CONDITIONAL, b3=silent (all 3 agree)
:Gate: --enable-heterogeneous (OPAL_ENABLE_HETEROGENEOUS_SUPPORT, default 0)
:Evidence: config/opal_configure_options.m4:360-374 (default disabled); ompi/proc/proc.c:267-274 (OMPI_ARCH published to modex only under #if OPAL_ENABLE_HETEROGENEOUS_SUPPORT) and proc.c:781-795 (differing peer arch -> OMPI_ERR_NOT_SUPPORTED + 'heterogeneous-support-unavailable' in non-hetero build); opal/datatype/opal_convertor.c (byte-swap/conversion engaged only when remoteArch != opal_local_arch)
:Notes: Unanimous CONDITIONAL; this is the framework's named example. Conversion behavior is genuinely gated behind the non-default --enable-heterogeneous flag. Default homogeneous build is conformant (no conversion needed) but does not provide cross-representation conversion; a true arch mismatch is refused rather than mis-converted.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Blocking standard-mode send/recv (MPI_Send/MPI_Recv): envelope matching on (source,tag,comm), MPI_ANY_SOURCE/MPI_ANY_TAG wildcards, count/datatype semantics, truncation detection
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Three additional send modes: buffered (Bsend), synchronous (Ssend, nonlocal rendezvous), ready (Rsend), blocking and nonblocking
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_TAG_UB >= 32767 requirement
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Return status semantics: MPI_SOURCE/MPI_TAG/MPI_ERROR fields and MPI_Get_count decode (MPI_UNDEFINED on non-divisible/overflow; 0 on zero-size datatype)
     - b1=SUPP/b2=sile/b3=SUPP (partial-overlap)
   * - MPI_STATUS_IGNORE / MPI_STATUSES_IGNORE accepted wherever status is an OUT argument
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Blocking send-receive (MPI_Sendrecv) and in-place replace (MPI_Sendrecv_replace)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Type-matching rules: identical-name match, MPI_BYTE matches any byte, MPI_PACKED matches any type (homogeneous case)
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Ordering guarantee: messages are non-overtaking (single-threaded determinism), progress, and (no) fairness
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Multithreaded ordering relaxation / concurrent send-recv from distinct threads under MPI_THREAD_MULTIPLE
     - b1=COND/b2=sile/b3=sile (unique)
   * - Per-process buffered-mode buffer (MPI_Buffer_attach/detach) with BSEND_OVERHEAD accounting and circular allocation
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI-5.0 buffer-policy overhaul: per-communicator and per-session Bsend buffers ({Comm,Session}_{attach,detach}_buffer) with precedence comm > session > process
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_BUFFER_AUTOMATIC: library auto-sizes buffering for buffered-mode sends at any level (C and Fortran sentinel)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Blocking flush of buffered-mode buffers (MPI_Buffer_flush, MPI_Comm_flush_buffer, MPI_Session_flush_buffer): nonlocal, drains before return
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Nonblocking flush (MPI_Buffer_iflush, MPI_Comm_iflush_buffer, MPI_Session_iflush_buffer): start a flush whose returned request completes once buffered messages are transmitted
     - b1=PART/b2=SUPP/b3=SUPP (conflict)
   * - Nonblocking communication: Isend/Ibsend/Issend/Irsend (per-mode), Irecv, Isendrecv/Isendrecv_replace start with separate completion
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Single completion: MPI_Wait / MPI_Test and MPI_Request_free for active/persistent requests
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Non-destructive status test: MPI_Request_get_status and _any/_all/_some
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Probe operations: MPI_Probe (blocking) and MPI_Iprobe (nonblocking) match without receiving
     - b1=SUPP/b2=sile/b3=SUPP (partial-overlap)
   * - Matched probe/receive: MPI_Mprobe/MPI_Improbe yield an MPI_Message; MPI_Mrecv/MPI_Imrecv consume it (thread-safe matched receive; MPI_MESSAGE_NO_PROC for MPI_PROC_NULL)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Cancel of a pending (unmatched) receive request and MPI_Test_cancelled reporting
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Cancel of a send request (deprecated, best-effort)
     - b1=PART/b2=SUPP/b3=SUPP (conflict)
   * - Persistent communication requests: per-mode init (Send/Bsend/Ssend/Rsend_init, Recv_init), MPI_Start, MPI_Startall
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Null-process semantics: send/recv to/from MPI_PROC_NULL succeed immediately; recv status returns source=PROC_NULL, tag=ANY_TAG, count=0
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Chapter-3 error classes exist and are returned: MPI_ERR_TRUNCATE, MPI_ERR_IN_STATUS, MPI_ERR_PENDING, MPI_ERR_BUFFER, MPI_ERR\_{REQUEST,TYPE,COUNT,TAG,RANK}
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Point-to-point delivered by the default transport stack (no optional library required); big-count (_c) entry points present
     - b1=SUPP/b2=sile/b3=SUPP (partial-overlap)
