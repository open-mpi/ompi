.. _release-notes-mpi-5.0-ch06:

Collective Communication
========================


.. admonition:: tl;dr
   :class: tip

   Open MPI's main branch implements MPI-5.0 Chapter 6 by default. All blocking collectives (barrier, bcast, gather[v], scatter[v], allgather[v], alltoall[v][w], reduce, allreduce, reduce_scatter[_block], scan, exscan) plus neighbor collectives are served by the unconditionally-built basic component (intra+inter paths) and accelerated by tuned/han/inter; all nonblocking (I\*) and persistent (\*_init) variants are served by libnbc, which has no configure.m4 and is always built. Predefined and user-defined ops, MPI_Reduce_local, MPI_IN_PLACE, big-count _c variants, op/datatype validity checks (MPI_ERR_OP), and inter-communicator two-group semantics are all default-supported. b1 and b2 cover the full chapter; b3 scoped itself to blocking collectives and explicitly deferred nonblocking/persistent to other agents (silent there, not disagreement). Two real edge findings: (1) MPI_Type_get_value_index implements only the predefined NAMED value-index pairs (FLOAT_INT, 2INT, Fortran 2INTEGER/2REAL, etc.) and returns MPI_DATATYPE_NULL for unnamed pairs (Example 6.17's MPI_DOUBLE+MPI_UINT64_T); MPI_COMBINER_VALUE_INDEX is defined as a constant but never produced by the envelope path -> PARTIAL. (2) MPI_Exscan does not reject an inter-communicator argument (MPI_Scan does) -> minor permissiveness, the required intra-comm behavior is fully supported. Predefined-op reductions over REAL16/16-byte long double are gated on optional compiler support (CONDITIONAL). Optional accelerated providers (UCC/Portals4/accelerator/acoll) are non-required layers (CONDITIONAL), never the sole provider of any required behavior.

Conformance summary
-------------------


**21** reconciled requirement(s)/behavior(s): 17 supported, 2 conditional, 2 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (81.0% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI_Exscan does not reject an inter-communicator argument
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §6.2.2, §6.11.2 / p.191, p.255
:Reviewers: b1=silent, b2=PARTIAL, b3=silent (raised by 1 reviewer)
:Evidence: ompi/mpi/c/exscan.c.in:64-65 (comment 'same checks for intracommunicators and intercommunicators'); no OMPI_COMM_IS_INTER -> MPI_ERR_COMM branch, unlike scan.c.in:68
:Notes: Unique to b2 (it filed this as a 'minor' caveat, not a missing_gap). Verified: MPI_Exscan lacks the inter-comm rejection that MPI_Scan has, whereas the standard types the comm argument as intra-communicator. This is over-permissiveness on an erroneous argument, not a missing required behavior — the required intra-comm exscan is fully supported. Classed PARTIAL on this narrow validation point; does not break conforming programs.

MPI_Type_get_value_index: UNNAMED predefined value-index pairs (Example 6.17, e.g. MPI_DOUBLE+MPI_UINT64_T) yielding combiner MPI_COMBINER_VALUE_INDEX
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §6.9.4 / p.238 (Example 6.17)
:Reviewers: b1=silent, b2=UNSUPPORTED, b3=silent (raised by 1 reviewer)
:Evidence: ompi/datatype/ompi_datatype_create.c:128-131 (comment: 'not a complete implementation ... doesn't support possible unnamed datatypes'); :136 returns MPI_DATATYPE_NULL for any non-named pair; MPI_COMBINER_VALUE_INDEX defined only as a constant at ompi/include/mpi.h.in:837 (and mpif-constants.h:228, mpif-values.py:329) and never produced/consumed in datatype envelope code (no VALUE_INDEX case in ompi_datatype_args.c)
:Notes: Unique to b2 (filed missing_gap). Confirmed in source: unnamed value-index pairs return MPI_DATATYPE_NULL instead of a usable type, and MPI_COMBINER_VALUE_INDEX is defined but never emitted by the get_envelope path. Core libmpi code compiled unconditionally (gate none), not touched by any in-flight PR (#13280=ABI, ompio-info=File hints). Classed PARTIAL rather than UNSUPPORTED: MPI_Type_get_value_index exists and returns the predefined named pairs correctly; only the unnamed-pair subset and its VALUE_INDEX envelope are unimplemented — present-but-incomplete = PARTIAL per taxonomy.

Predefined-op reductions over MPI_REAL16 / 16-byte long double
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §6.9.2 / p.232
:Reviewers: b1=CONDITIONAL, b2=SUPPORTED, b3=silent (reviewers disagreed)
:Gate: compiler support: OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C (optional compiler feature: matching 16-byte real KIND)
:Evidence: ``ompi/mca/op/base/op_base_functions.c:210,276,339,411 (REAL16/c_long_double_complex functions guarded by #if OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C)``
:Notes: Conflict b1(conditional) vs b2(default). b1 is correct under the framework: REAL16 reductions are compiled only when the compiler provides a matching 16-byte real KIND — an optional compiler feature, which the framework lists as CONDITIONAL. Scalar long double reductions work regardless; only the Fortran REAL16 path is gated. Not a conformance defect, a compiler limitation.

Optional hardware-accelerated / offloaded collective providers (UCC, Portals4, accelerator/GPU, AMD acoll, han/adapt/xhc)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §6.1 (advice to implementors) / p.190
:Reviewers: b1=CONDITIONAL, b2=CONDITIONAL, b3=CONDITIONAL (all 3 agree)
:Gate: coll/ucc (--with-ucc / UCC lib), coll/portals4 (Portals4), coll/accelerator (CUDA/ROCm via OMPI_HAVE_ACCELERATOR_SUPPORT), coll/acoll; han/adapt/xhc/tuned are default-on accelerators
:Evidence: ompi/mca/coll/ucc/configure.m4, portals4/configure.m4, accelerator/configure.m4 (component build gates); basic/tuned/libnbc provide all required behavior regardless
:Notes: Unanimous. These are additional algorithm providers layered above the always-built basic/tuned/libnbc stack; never the sole provider of a required behavior, so disabling them does not affect conformance. UCC/Portals4/accelerator require optional external deps -> CONDITIONAL; han/adapt/tuned/xhc are default-on (their removal is gate-only).

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Blocking barrier (intra + inter), MPI_Barrier
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Blocking data-movement collectives over intra-communicators (bcast, gather[v], scatter[v], allgather[v], alltoall[v][w]) with type-matching / rank-order semantics
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Blocking collectives over inter-communicators (MPI_ROOT/MPI_PROC_NULL root convention, two-group all-to-all semantics)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Reduction family (reduce, allreduce, reduce_scatter[_block], scan, exscan) with predefined ops
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Predefined-op x datatype coverage incl. MAXLOC/MINLOC over the standard type groups
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - User-defined reduction ops (MPI_Op_create/_c, MPI_Op_free, MPI_Op_commutative; commute flag honored; C + Fortran callbacks)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Op/datatype validity check: reject invalid op/datatype combos and MPI_OP_NULL with MPI_ERR_OP
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - MPI_Reduce_local (process-local reduction, no communicator)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_IN_PLACE semantics for collectives (blocking/nonblocking/persistent)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Nonblocking collectives (all MPI_I\* variants) with separate completion and independent progress
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Ibarrier is the one synchronizing nonblocking collective
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Persistent collectives (all MPI\_\*_init variants) producing reusable inactive requests started by MPI_Start/Startall, with info argument
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info hints accepted on persistent collective init calls
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Large-count (_c / MPI_Count) collectives carried end-to-end without int truncation (blocking, nonblocking, persistent)
     - b1=SUPP/b2=sile/b3=SUPP (unanimous)
   * - Scan/Exscan restricted to intra-communicators; MPI_Scan rejects inter-comm
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Type_get_value_index: predefined NAMED value-index pair types (FLOAT_INT, DOUBLE_INT, 2INT, Fortran 2INTEGER/2REAL...), NULL-with-SUCCESS for no match
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Mixing nonblocking/persistent collective requests with other requests; MPI_CANCEL / active MPI_REQUEST_FREE on collective requests erroneous
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
