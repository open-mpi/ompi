.. _release-notes-mpi-5.0-ch13:

External Interfaces
===================


.. admonition:: tl;dr
   :class: tip

   Open MPI's main branch implements MPI-5.0 Chapter 13 by default in C: generalized requests (MPI_Grequest_start/complete plus the three query/free/cancel callbacks with correct deferred-free, cancel-complete-flag, and error-code propagation semantics), all status setters (MPI_Status_set_elements/_x, set_cancelled, set_source/tag/error), the six status conversion routines (f2c/c2f/c2f08/f082c/f2f08/f082f), MPI_Request_get_status{,_any,_some,_all}, datatype decoding (MPI_Type_get_envelope/contents +_c), and thread support (Init_thread/Query_thread/Is_thread_main with provided=requested up to MPI_THREAD_MULTIPLE, ungated). All of this is unconditional default code; --enable-grequest-extensions gates only the non-standard MPIX extension, not standard grequests. The one genuine gap is the Fortran (mpi_f08/mpi-module/mpif.h) binding for all six MPI_Status\_{get,set}_{source,tag,error} routines: only C bindings exist on main, and no in-flight PR adds them. The underlying C behavior is reachable, but the standard-mandated Fortran bindings are absent (UNSUPPORTED), so b1's PARTIAL framing dissolves into a SUPPORTED C-behavior topic plus an UNSUPPORTED f08-binding topic.

Conformance summary
-------------------


**15** reconciled requirement(s)/behavior(s): 15 supported, 0 conditional, 0 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (100.0% of applicable fully supported).

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - MPI_Grequest_start creates a generalized request storing the three callbacks + extra_state
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - query_fn invoked by completing MPI\_{Wait,Test}\* and by MPI_Request_get_status; callback status returned to user; return code propagated to MPI_ERROR only on failure
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - free_fn invoked after query_fn for completing call; order-independent deferred-free (object freed only after both MPI_REQUEST_FREE and MPI_GREQUEST_COMPLETE; free_fn runs exactly once); MPI call returns free_fn code
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - cancel_fn invoked by MPI_Cancel with complete=true iff MPI_GREQUEST_COMPLETE already called; C and Fortran callbacks both handled
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Both C and Fortran callback variants supported (C/F dispatch with ierr/logical conversion)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Grequest_complete marks request complete so MPI_Wait returns / MPI_Test flag=true and may unblock a waiting thread
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Status_set_elements (+ _c large-count and deprecated _x) sets opaque _ucount so MPI_Get_elements/Get_count return compatible values; handles predefined vs derived types and STATUS_IGNORE
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Status_set_cancelled sets opaque cancel flag so MPI_Test_cancelled returns it (C + f08)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Status_get/set_source/tag/error: C semantic behavior (read/write the directly accessible MPI_SOURCE/MPI_TAG/MPI_ERROR status fields)
     - b1=PART/b2=SUPP/b3=SUPP (conflict)
   * - Generalized-request / status-set procedures built by default (not gated on optional MCA component, configure flag, thread level, or 3rd-party lib)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Request_get_status{,_any,_some,_all} report completion (and invoke query_fn) without freeing the request
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Status conversion routines MPI_Status_f2c/c2f (deprecated) and c2f08/f082c/f2f08/f082f
     - b1=sile/b2=sile/b3=sile (partial-overlap)
   * - Datatype decoding: MPI_Type_get_envelope / MPI_Type_get_contents (+ _c large-count)
     - b1=sile/b2=sile/b3=sile (partial-overlap)
   * - Thread support: MPI_Init_thread provided level, MPI_Query_thread, MPI_Is_thread_main, the four THREAD\_\* levels
     - b1=sile/b2=sile/b3=sile (partial-overlap)
   * - MPI_Type_create_keyval / MPI_Comm_create_keyval (attribute keyval creation referenced by external-interfaces investigation)
     - b1=sile/b2=sile/b3=sile (partial-overlap)
