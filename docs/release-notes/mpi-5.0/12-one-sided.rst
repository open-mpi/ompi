.. _release-notes-mpi-5.0-ch12:

One-Sided Communications (RMA)
==============================


.. admonition:: tl;dr
   :class: tip

   Open MPI's MPI-5.0 Chapter 12 (RMA) support is overwhelmingly SUPPORTED by default. The osc framework ships a built-in, ungated default path: osc/rdma (priority 20, builds over any BTL with a software lock-based atomic fallback) for general windows, and osc/sm (priority 100, no configure.m4) for shared-memory windows. All RMA procedures (window create/allocate/allocate_shared/create_dynamic, attach/detach, Put/Get/Accumulate/Get_accumulate/Fetch_and_op/Compare_and_swap, the R-variants, Fence/PSCW/Lock(_all)/Flush(_local)(_all)/Sync) are present in both C and use-mpi-f08 bindings, and all twelve RMA error classes are defined. User-defined ops are rejected with MPI_ERR_OP at the C binding (accumulate.c.in:67-68); REPLACE/NO_OP are special-cased. Memory model reports MPI_WIN_UNIFIED (fully conformant; SEPARATE is not required) and osc/portals4 can report SEPARATE. UCX (osc/ucx), Portals4 (osc/portals4), and UBCL (osc/ubcl) are OPTIONAL accelerators gated on external libraries, not required for conformance. The single genuine deviation is PARTIAL: MPI_Win_get_info on the default rdma path does not synthesize default values for honored-but-unsubscribed hints (same_size, same_disp_unit, accumulate_ops); only no_locks is subscribed and round-tripped (TODO acknowledged at osc_rdma_component.c:1518). User-supplied non-ignored hints DO round-trip. Two ignorable info hints have no effect on the default path: accumulate_ordering is parsed under the misspelled key 'accumulate_order' with zero consumers in osc/rdma, and mpi_accumulate_granularity is recognized by no osc component; both are permitted to be ignored (Sec 12.2.7) and the conformant strict default ordering is always enforced. Nothing in this chapter is in-flight (PR #13280 ABI and the OMPIO MPI_File-info branch do not touch ch12).

Conformance summary
-------------------


**33** reconciled requirement(s)/behavior(s): 29 supported, 3 conditional, 1 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (87.9% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI_WIN_GET_INFO returns all supported hints WITH DEFAULT VALUES (default-value synthesis)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §12.2.7 / p.561
:Reviewers: b1=SUPPORTED, b2=SUPPORTED, b3=PARTIAL (reviewers disagreed)
:Evidence: ompi/mca/osc/rdma/osc_rdma_component.c:1515 (subscribes only 'no_locks'), :1517-1520 (TODO: same_size/same_disp_unit not subscribed); opal/util/info.c:79 (opal_info_dup_public skips keys with ie_referenced==0); osc_rdma_component.c:1416-1417 (rdma reads same_size/same_disp_unit via check_config_value_bool -> opal_info_get, which refs only USER-supplied keys, info.c:124)
:Notes: CONFLICT resolved to PARTIAL, siding with b3 (medium-confidence minority). The standard requires get_info to return all hints 'supported by the implementation and have default values specified.' On the default rdma path, only no_locks is subscribed and thus injected with a default into s_info, so it round-trips. same_size/same_disp_unit/accumulate_ops are honored internally but have NO default injected, so their DEFAULTS are not reported by MPI_Win_get_info (developer TODO at :1518 acknowledges this). User-supplied non-ignored hints DO round-trip (check_config_value_bool/enum-get call opal_info_get which increments ie_referenced). Scope is narrow: procedures work, user hints round-trip; the gap is only default-value synthesis for honored-but-unsubscribed hints. Does not affect RMA data-movement correctness. b1/b2 (SUPPORTED) overweighted the working procedure path; the TODO + dup_public ie_referenced==0 skip make PARTIAL the honest call. Not in-flight (inflight.json covers only ABI #13280 and OMPIO MPI_File-info).

UCX-accelerated RMA (osc/ucx) as optional provider
""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §12.\* (implementation choice)
:Reviewers: b1=CONDITIONAL, b2=CONDITIONAL, b3=CONDITIONAL (all 3 agree)
:Gate: UCX library via --with-ucx (optional accelerator; baseline RMA still works via default rdma/sm)
:Evidence: ``ompi/mca/osc/ucx/configure.m4 (OMPI_CHECK_UCX gate, builds only when osc_ucx_happy=yes); osc_ucx_component.c:173 (priority 60 only for UCX>=1.5 else 0)``
:Notes: Optional accelerator, not required. CONDITIONAL on UCX. Unanimous, verified; baseline RMA unaffected.

Portals4 RMA (osc/portals4) as optional provider
""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §12.\* (implementation choice)
:Reviewers: b1=CONDITIONAL, b2=CONDITIONAL, b3=CONDITIONAL (all 3 agree)
:Gate: Portals4 library (optional accelerator; baseline RMA still works via default rdma/sm)
:Evidence: ``ompi/mca/osc/portals4/configure.m4 (OPAL_CHECK_PORTALS4 gate); osc_portals4_component.c:401 (SHARED flavor returns OMPI_ERR_NOT_SUPPORTED)``
:Notes: Optional accelerator. Does not support SHARED flavor (other components cover it). Reports UNIFIED or SEPARATE. CONDITIONAL on Portals4. Unanimous, verified.

UBCL RMA (osc/ubcl) as optional provider
""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §12.\* (implementation choice)
:Reviewers: b1=CONDITIONAL, b2=CONDITIONAL, b3=silent (all 3 agree)
:Gate: UBCL library (optional accelerator; baseline RMA still works via default rdma/sm)
:Evidence: ompi/mca/osc/ubcl/configure.m4 (OMPI_CHECK_UBCL gate); osc_ubcl_accumulate.c (substantial accumulate paths return OMPI_ERR_NOT_IMPLEMENTED; GPU buffers unsupported)
:Notes: Optional accelerator with incomplete accumulate coverage; irrelevant to default-path conformance since baseline RMA is served by rdma/sm. CONDITIONAL on UBCL.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - RMA procedures present (C + mpi_f08 bindings) for all window/comm/sync ops
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - osc framework provides a default RMA code path (osc/rdma + osc/sm, ungated)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_WIN_CREATE over user memory (FLAVOR_CREATE), collective over comm
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_WIN_ALLOCATE returns MPI-allocated (symmetric) memory (FLAVOR_ALLOCATE)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_WIN_ALLOCATE_SHARED + MPI_WIN_SHARED_QUERY (FLAVOR_SHARED), alloc_shared_noncontig
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_WIN_CREATE_DYNAMIC + MPI_WIN_ATTACH / MPI_WIN_DETACH (dynamic windows)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_WIN_FREE collective destruction (delays for passive-target completion unless no_locks)
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Window attributes BASE/SIZE/DISP_UNIT/CREATE_FLAVOR/MODEL cached and queryable
     - b1=SUPP/b2=sile/b3=SUPP (unanimous)
   * - Window user-keyval / attribute machinery (Win_create_keyval/free_keyval/set_attr/get_attr/delete_attr + copy/delete callback typedefs)
     - b1=sile/b2=sile/b3=NA (unique)
   * - MPI_PUT / MPI_GET data transfer (general datatypes, MPI_PROC_NULL target)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_ACCUMULATE element-wise atomic update; predefined ops + MPI_REPLACE only; reject user-defined ops
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_GET_ACCUMULATE / MPI_FETCH_AND_OP / MPI_COMPARE_AND_SWAP atomics (MPI_NO_OP / MPI_REPLACE)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Request-based RMA (MPI_RPUT/RGET/RACCUMULATE/RGET_ACCUMULATE) completed via MPI test/wait
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Memory model: window reports MPI_WIN_UNIFIED or MPI_WIN_SEPARATE via MPI_WIN_MODEL
     - b1=PART/b2=SUPP/b3=SUPP (conflict)
   * - Active-target sync: MPI_WIN_FENCE
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Active-target PSCW: MPI_WIN_START/COMPLETE/POST/WAIT/TEST
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Passive-target locks: MPI_WIN_LOCK/UNLOCK/LOCK_ALL/UNLOCK_ALL (EXCLUSIVE & SHARED)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Flush ops: MPI_WIN_FLUSH/FLUSH_ALL/FLUSH_LOCAL/FLUSH_LOCAL_ALL
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_WIN_SYNC public/private window copy reconciliation
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Assert mode hints (NOCHECK/NOSTORE/NOPUT/NOPRECEDE/NOSUCCEED) accepted on sync calls
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - RMA usable under MPI_THREAD_MULTIPLE
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - RMA error classes (Table 12.2) defined and raised
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_WIN_SET_INFO / MPI_WIN_GET_INFO procedures present and functional
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - no_locks info hint recognized and honored
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - accumulate_ops info hint (same_op / same_op_no_op) relaxes atomicity protection
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - same_size and same_disp_unit info hints recognized
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - accumulate_ordering info hint (relax RMA accumulate ordering at target)
     - b1=PART/b2=UNSU/b3=sile (conflict)
   * - mpi_accumulate_granularity info hint
     - b1=sile/b2=UNSU/b3=sile (unique)
   * - mpi_assert_memory_alloc_kinds hint honored on windows
     - b1=sile/b2=SUPP/b3=sile (unique)
