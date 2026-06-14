.. _release-notes-mpi-5.0-ch18:

Semantic Changes and Warnings
=============================


.. admonition:: tl;dr
   :class: tip

   Chapter 18 introduces no new symbols; it records behavioral changes carried in from MPI-4.0/4.1 plus advice-to-users text. b1 and b2 cover it; b3 was silent on this chapter entirely (it merged 16+17 and never addressed 18) -- silence is not disagreement. Three of the four behavioral items are SUPPORTED by default: MPI_Comm_dup/idup no longer propagate input-communicator info hints (the \*_with_info variants are the explicit carry path), and MPI_Wtime/MPI_Wtick (plus PMPI\_ forms) and all handle-conversion functions are real OMPI_DECLSPEC functions, never macros. The one genuinely CONDITIONAL item is the MPI-4.0 default-error-communicator change: routing errors that involve no comm/win/file to MPI_COMM_SELF (instead of the legacy MPI_COMM_WORLD) is REQUIRED by MPI-5.0 and is compiled into every build, but is gated OFF by default -- the READONLY MCA parameter mpi_compat_mpi3 defaults to true (verified ompi/runtime/ompi_mpi_params.c:86,366), so out of the box Open MPI raises such errors on MPI_COMM_WORLD. The conformant behavior is reachable with zero rebuild by launching with mpi_compat_mpi3=false, so it is CONDITIONAL (required behavior on a non-default path), not a gap. This is the exact inverse of the Chapter 17 gating: same mechanism (a non-default flag), opposite requiredness -> opposite status. The 30/31-character identifier-length relaxation is pure advice-to-users with no implementation obligation -> NA.

Conformance summary
-------------------


**4** reconciled requirement(s)/behavior(s): 2 supported, 1 conditional, 0 partial, 0 unsupported, 0 missing-but-in-flight, 1 N/A (66.7% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


Errors not involving a communicator, window, or file must be raised on MPI_COMM_SELF (changed from MPI_COMM_WORLD in MPI-4.0); MPI-5.0 mandates the MPI_COMM_SELF default
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §18.1.1 / p.831
:Reviewers: b1=CONDITIONAL, b2=CONDITIONAL, b3=silent (all 3 agree)
:Gate: MCA runtime param mpi_compat_mpi3=false (READONLY, set at launch; default is true)
:Evidence: ompi/errhandler/errhandler_invoke.c:54 (comm = ompi_mpi_compat_mpi3 ? &ompi_mpi_comm_world.comm : &ompi_mpi_comm_self.comm; verified); ompi/runtime/ompi_mpi_params.c:86 (bool ompi_mpi_compat_mpi3 = true) and :366-372 (re-set true, registered READONLY with help text: true=raise on MPI_COMM_WORLD/MPI-3, false=raise on MPI_COMM_SELF/MPI-4)
:Notes: b1 and b2 unanimous CONDITIONAL; verified directly. The MPI-5.0-conformant routing to MPI_COMM_SELF is compiled in but gated OFF: mpi_compat_mpi3 defaults to true (no configure-derived override; the only routing point for the running-MPI state is line 54). Out-of-the-box, Open MPI is non-conformant on this point and raises no-handle errors on MPI_COMM_WORLD. Required behavior reachable only via a non-default runtime param = CONDITIONAL (not a gap: zero rebuild needed; not default: explicit opt-in needed). Neither in-flight PR touches this. Contrast Ch17, where the gated behavior is NOT required, so that gating yields SUPPORTED; here it IS required, so the gating yields CONDITIONAL.

The 30/31-character limit on MPI identifiers was removed (MPI-4.0 warning); a relaxation with no implementation obligation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **N/A**
:Standard: §18.2.2 / p.832
:Reviewers: b1=SUPPORTED, b2=NA, b3=silent (reviewers disagreed)
:Evidence: no source artifact; Open MPI never enforced a 30/31-char identifier limit and freely uses public identifiers longer than 31 chars (e.g. MPI_Comm_idup_with_info, mpi.h.in:1634)
:Notes: CONFLICT in label only: b1 called it SUPPORTED but its own caveat says 'This is a relaxation, not an implementation requirement; no implementation constraint exists ... No single file:line implements it' -- which is exactly NA. b2 marked it NA. Resolved to NA per the framework's NA clause (advice-to-users / requirement that does not bind an implementation). The relaxation is trivially satisfied; there is nothing to implement.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - MPI_Comm_dup / MPI_Comm_idup do NOT propagate info hints from the input communicator to the output (MPI-4.0 semantic change); the explicit \*_with_info variants are the prescribed path to carry info
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Wtick / PMPI_Wtick / MPI_Wtime / PMPI_Wtime, and all handle-conversion functions (MPI\_\*_c2f / MPI\_\*_f2c), must NOT be implemented as macros (MPI-4.1 warning)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
