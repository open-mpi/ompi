.. _release-notes-mpi-5.0-ch02:

MPI Terms and Conventions
=========================


.. admonition:: tl;dr
   :class: tip

   Chapter 2 is definitional/conventional and carries no assigned MPI procedures or constants of its own; conformance is assessed semantically against cross-cutting requirements. Open MPI satisfies almost every Chapter-2 requirement by default: opaque-object handle types (C typedefs + USE mpi_f08 BIND(C) derived types with a single INTEGER MPI_VAL and overloaded .EQ./.NE./==//=), invalid-handle (\*_NULL) and reserved-sentinel constants, MPI_Aint/MPI_Offset/MPI_Count C types with matching Fortran KIND parameters, the large-count _c procedure family, the AINT_ADD/DIFF-only macro rule, default ERRORS_ARE_FATAL on comms/wins with ERRORS_RETURN on files, the three predefined error handlers, the weak-progress guarantee via in-call opal_progress() polling, MPI_BOTTOM/MPI_IN_PLACE/MPI_STATUS_IGNORE/MPI_PROC_NULL buffer-and-status conventions, the MPI_THREAD_SINGLE..MULTIPLE compliance model, COMM_DUP info non-propagation, and the signal-usage documentation obligation. Two requirements are CONDITIONAL: (1) the MPI-1 constructs Table 2.1 marks 'Removed since MPI-3.0' are compiled out of mpi.h by default and source-usable only with the non-default --enable-mpi1-compatibility (the standard permits omitting them, so this is optional); (2) the MPI-4/5 rule that handle-less errors raise on MPI_COMM_SELF is implemented but NOT the shipped default -- Open MPI ships ompi_mpi_compat_mpi3=true, raising such errors on MPI_COMM_WORLD (MPI-3 behavior) unless the user sets the non-default MCA var mpi_compat_mpi3=0. Strong/asynchronous progress is explicitly not required by the standard (NA). No Chapter-2 behavior is missing or in-flight; the two in-flight PRs (ABI ch.20, ompio-info ch.13) do not bear on this chapter.

Conformance summary
-------------------


**22** reconciled requirement(s)/behavior(s): 19 supported, 2 conditional, 0 partial, 0 unsupported, 0 missing-but-in-flight, 1 N/A (90.5% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI-1 constructs Table 2.1 marks 'Removed since MPI-3.0' (MPI_UB/LB, MPI_Address, MPI_Type_hvector/hindexed/struct/extent, MPI_Errhandler_create/get/set, \*_INTEGER combiners) may be provided for backward compatibility but are not required
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §2.6.1 / Table 2.1 / p.24
:Reviewers: b1=silent, b2=CONDITIONAL, b3=silent (raised by 1 reviewer)
:Gate: configure flag --enable-mpi1-compatibility (default disabled; OMPI_ENABLE_MPI1_COMPAT / OMPI_OMIT_MPI1_COMPAT_DECLS)
:Evidence: config/ompi_configure_options.m4:179-192 (--enable-mpi1-compatibility default disabled -> ompi_mpi1_support=0); ompi/include/mpi.h.in:407-408 (OMPI_OMIT_MPI1_COMPAT_DECLS = !(OMPI_ENABLE_MPI1_COMPAT\|\|OMPI_BUILDING)); :809-846,1259-1274,4080-4091 (combiners/MPI_UB/MPI_LB/removed typedefs gated, else removed-symbol static assert)
:Notes: b2-unique, verified and confirmed. By default these removed names are compiled out of mpi.h (or expand to a removed-symbol static_assert on C11+); --enable-mpi1-compatibility restores them. The standard permits omitting them (may-be-provided-not-required), so default-omitted-with-optional-flag = CONDITIONAL. The .c impls remain linked for ABI but are undeclared by default, so this does not make them source-usable. Not in-flight (PR #13280 adds only ABI MPI_Abi/toint/fromint symbols).

Errors not attributable to a specific MPI object are raised on MPI_COMM_SELF (World Model, MPI-4/5 behavior)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §2.8 / p.26-27 (and §18.1.1 change)
:Reviewers: b1=SUPPORTED, b2=SUPPORTED, b3=CONDITIONAL (reviewers disagreed)
:Gate: MCA var mpi_compat_mpi3=0 (default is true -> MPI-3 MPI_COMM_WORLD behavior)
:Evidence: ompi/errhandler/errhandler_invoke.c:54 (comm = ompi_mpi_compat_mpi3 ? &ompi_mpi_comm_world.comm : &ompi_mpi_comm_self.comm); ompi/runtime/ompi_mpi_params.c:86,366-372 (ompi_mpi_compat_mpi3 = true by default; settable via mpi_compat_mpi3)
:Notes: CONFLICT resolved in b3's favor with source proof. b1 and b2 both claimed COMM_SELF is the default (both cited the COMM_SELF behavior as supported/default), but the shipped default is ompi_mpi_compat_mpi3=true, which routes handle-less errors to MPI_COMM_WORLD (MPI-3 behavior). The MPI-5-conformant MPI_COMM_SELF behavior is reachable ONLY by setting the non-default MCA var mpi_compat_mpi3=0. This is default-OFF-enableable (inverse of default-on-removable), directly parallel to --enable-mpi1-compatibility, so CONDITIONAL per the framework's b3-normalization rule. Not missing/in-flight; the code path exists.

Strong / asynchronous progress (dedicated progress thread or NIC offload)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **N/A**
:Standard: §2.9 / p.28
:Reviewers: b1=silent, b2=CONDITIONAL, b3=NA (reviewers disagreed)
:Evidence: ompi/mca/mtl/ofi/mtl_ofi_component.c:183,198 (control/data_progress default MTL_OFI_PROG_UNSPEC -> FI_PROGRESS_AUTO NOT requested by default); opal/runtime/opal_progress_threads.c:139 (progress-thread machinery; sole non-default caller is btl/usnic)
:Notes: CONFLICT (b2 CONDITIONAL vs b3 NA), resolved to NA. The standard EXPLICITLY does not require strong/asynchronous progress (2.9), so per the taxonomy 'optional feature MPI explicitly permits omitting' = NA. b2's CONDITIONAL over-classifies an above-baseline optional capability as a gated requirement. The capability does exist only via non-default transport/build/MCA-param choices, but since the standard does not mandate it, its absence-by-default is not a conformance matter.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Opaque objects accessed via typed handles; distinct C handle type per category; assignment and equality comparison
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - USE mpi_f08 handles are BIND(C) derived types with a single INTEGER MPI_VAL; .EQ./.NE./==//= overloaded for handle comparison
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Invalid-handle (MPI\_\*_NULL) constants and predefined static handles per object type; predefined objects must not be user-freed
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Named-constant / reserved-sentinel conventions (MPI_BOTTOM, MPI_IN_PLACE, MPI_UNWEIGHTED, MPI_WEIGHTS_EMPTY, MPI_BUFFER_AUTOMATIC, MPI_ERRCODES_IGNORE, MPI_ANY_TAG/SOURCE, MPI_PROC_NULL, MPI_UNDEFINED) as distinct out-of-range addresses/integers
     - b1=sile/b2=sile/b3=SUPP (unique)
   * - Special Fortran constants that cannot be PARAMETERs (MPI_BOTTOM, MPI_IN_PLACE, MPI_STATUS_IGNORE, etc.) implemented as predefined static/COMMON-block variables; all other named constants are PARAMETERs
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Aint / MPI_Offset / MPI_Count C types and matching Fortran INTEGER KIND parameters with identical width across languages; MPI_Count wide enough for int/MPI_Aint/MPI_Offset
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Large-count support via '_c'-suffixed C procedures (MPI_Send_c, MPI_Recv_c, MPI_Get_count_c, MPI_Type_contiguous_c, ...) and polymorphic mpi_f08 interfaces
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Only MPI_AINT_ADD/PMPI_AINT_ADD/MPI_AINT_DIFF/PMPI_AINT_DIFF may be implemented as C macros; no other MPI routine is a macro
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Deprecated-but-not-removed constructs (MPI_KEYVAL\_\*, MPI_ATTR\_\*, MPI_INFO_GET, MPI_SIZEOF, MPI_TYPE_SIZE_X, mpif.h) remain part of the standard and must be present
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - By default a detected error aborts the computation, EXCEPT file operations (comms/windows default ERRORS_ARE_FATAL; files default ERRORS_RETURN); three predefined handlers ARE_FATAL/RETURN/ABORT exist
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Errors before MPI_INIT / after MPI_FINALIZE / Sessions-only (no associated object, COMM_SELF uninitialized) routed to the launch-time initial error handler
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Weak-progress guarantee: decoupled MPI activities eventually execute during blocked procedures and repeated flag=false test calls
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Implementation documents which signals it uses; strongly recommended (soft) to avoid SIGALRM/SIGFPE/SIGIO; may prohibit MPI calls in signal handlers
     - b1=PART/b2=SUPP/b3=sile (conflict)
   * - MPI_BOTTOM and MPI_IN_PLACE buffer conventions honored where buffer args are expected
     - b1=sile/b2=sile/b3=SUPP (unique)
   * - MPI_STATUS_IGNORE / MPI_STATUSES_IGNORE / MPI_ERRCODES_IGNORE NULL-status convention (skip status writeback)
     - b1=sile/b2=sile/b3=SUPP (unique)
   * - MPI_PROC_NULL communication semantics: sends/recvs to/from PROC_NULL are no-ops returning MPI_SUCCESS with empty-request status on receive
     - b1=sile/b2=sile/b3=SUPP (unique)
   * - Thread-compliance model: MPI_THREAD_SINGLE..FUNNELED..SERIALIZED..MULTIPLE monotonic levels; MPI_Init_thread returns the provided level
     - b1=sile/b2=sile/b3=SUPP (unique)
   * - MPI_COMM_DUP / MPI_COMM_IDUP no longer propagate input-comm info hints (§18.1.1 semantic change)
     - b1=sile/b2=sile/b3=SUPP (unique)
   * - Basic runtime routines (printf, malloc, write) operate independently after MPI_INIT and before MPI_FINALIZE; per-process programs complete regardless of COMM_WORLD size
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
