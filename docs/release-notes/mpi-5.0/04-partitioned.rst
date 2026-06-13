.. _release-notes-mpi-5.0-ch04:

Partitioned Point-to-Point Communication
========================================


.. admonition:: tl;dr
   :class: tip

   Open MPI on this checkout (HEAD 71d6dcf7f4) implements MPI-5.0 Chapter 4 in the default build. All six procedures (MPI_Psend_init, MPI_Precv_init, MPI_Pready, MPI_Pready_range, MPI_Pready_list, MPI_Parrived) are present in both C and mpi_f08 bindings, backed by the always-built, non-optional 'part' framework whose sole 'persist' component is hard-selected with priority 1 (no configure flag, MCA, or external-library gate; works over any PML). Init calls are local/non-blocking with lazy negotiation; Start/complete reuses the Chapter 3 persistent-request machinery; Pready_range/Pready_list reduce to repeated Pready; the persist component handles differing send/recv partition counts via fractional aggregation. The one genuine deviation is MPI_Parrived on a null or freshly-inactive (never-started) request: the standard (p.119) mandates flag=true, but the default PARAM_CHECK build returns MPI_ERR_REQUEST for a null request (parrived.c.in:48) and flag=false for a freshly-initialized precv request (req->flags==NULL, part_persist.h:377->570). The core probing path is fully conformant and completed-then-reset inactive requests do return flag=true, so MPI_Parrived is PARTIAL, not absent. Neither in-flight item (ABI PR #13280, OMPIO MPI_File info) touches this chapter. The part.h header's 'blocking in the RMA component' note and the referenced-but-absent rma backend are vestigial and do not describe the shipped persist module.

Conformance summary
-------------------


**12** reconciled requirement(s)/behavior(s): 9 supported, 0 conditional, 1 partial, 0 unsupported, 0 missing-but-in-flight, 2 N/A (90.0% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI_Parrived with a null or inactive request must return flag=true
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §4.2.2 / p.119
:Reviewers: b1=PARTIAL, b2=UNSUPPORTED, b3=PARTIAL (reviewers disagreed)
:Evidence: ompi/mpi/c/parrived.c.in:48 (NULL request or non-PART req_type -> MPI_ERR_REQUEST in the default PARAM_CHECK build; MPI_REQUEST_NULL has req_type OMPI_REQUEST_NULL, not OMPI_REQUEST_PART); ompi/mca/part/persist/part_persist.h:377 (precv_init sets req->flags = NULL) -> :570 (mca_part_persist_parrived gates on '0 != req->flags', returns _flag=false at :567/589 for a freshly-init'd request).
:Notes: Conflict resolved to PARTIAL. MPI-5.0 p.119 requires flag=true for BOTH a null and an inactive request argument. Verified: default build returns MPI_ERR_REQUEST for null and flag=false for a freshly-initialized (never-started) precv (flags==NULL). However the procedure is PRESENT and its core is fully conformant, and a completed-then-reset inactive request DOES return flag=true (its flags array is retained) — so this is 'present but not fully standard-conformant' = PARTIAL, not UNSUPPORTED (b2's missing_gap normalizes to UNSUPPORTED but the behavior is present, not absent) and not SUPPORTED (no default path satisfies the MUST for the null / freshly-inactive cases). Not in-flight (in-flight list is only ABI ch20 + OMPIO MPI_File info). b1/b3 land on PARTIAL; adopted.

Absolute displacements / MPI_BOTTOM not supported for partitioned communication
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **N/A**
:Standard: §4.2.3 / p.119
:Reviewers: b1=SUPPORTED, b2=SUPPORTED, b3=silent (partial overlap)
:Evidence: ompi/mca/part/persist/part_persist.h:251,278 (per-partition buffer = req_addr + bytes\*i, assumes a concrete base address); standard explicitly disallows MPI_BOTTOM for partitioned ops.
:Notes: b1+b2 marked SUPPORTED, but the requirement is that MPI_BOTTOM/absolute displacements are NOT supported here — i.e. the standard itself permits the implementation to omit them. This is non-binding on the implementation, so NA is the precise taxonomy value (no required behavior to implement). Open MPI simply assumes a real buffer base and neither special-cases nor must reject MPI_BOTTOM. Status difference is purely vocabulary, not a substantive disagreement.

Erroneous-usage cases (partitions<=0, partition>=count, Pready on active partition, Pready on recv / Parrived on send request, free/cancel of an active request) need not be detected
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **N/A**
:Standard: §4.2 / p.116-120 (erroneous)
:Reviewers: b1=silent, b2=SUPPORTED, b3=SUPPORTED (partial overlap)
:Evidence: ompi/mpi/c/pready.c.in:48 / parrived.c.in:48 (check only handle non-NULL + req_type==OMPI_REQUEST_PART); ompi/mca/part/persist/part_persist_sendreq.c:32 & part_persist_recvreq.c:31 (req_cancel = NULL, consistent with cancel of a partitioned request being erroneous).
:Notes: b2 (ch04-erroneous-conditions, default) + b3 (erroneous-input caveat, fully_conformant) agree these are user errors the standard does not require the implementation to detect; b1 noted minimal validation in passing. NA is the precise value: the standard imposes no requirement for erroneous program input, so there is no binding behavior to implement. Verified req_cancel is wired to NULL for both send and recv part requests.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Six partitioned procedures present in C and mpi_f08 bindings (Psend_init, Precv_init, Pready, Pready_range, Pready_list, Parrived)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Partitioned framework/component built and selected by default (no optional library or configure-flag gate)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Partitioned init calls (Psend_init/Precv_init) are local/non-blocking; matching may be deferred (lazy negotiation)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Start/Startall activates the operation and resets partitions; Wait/Test complete via Chapter 3 request machinery
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Pready marks a send-side partition ready; valid only on a partitioned (send) request
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Pready_range and MPI_Pready_list are equivalent to repeated MPI_Pready over the indicated partitions
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Parrived probes partial receive completion (per-partition); does not complete the request; repeatable; aggregates across differing send/recv partition counts
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - count/datatype describe one partition; partitions equal-sized, contiguous, no padding; total data = parts\*count\*datatype
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Per-operation info argument accepted on Psend_init/Precv_init (implementation-defined keys)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
