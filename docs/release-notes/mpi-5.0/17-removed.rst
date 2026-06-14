.. _release-notes-mpi-5.0-ch17:

Removed Interfaces
==================


.. admonition:: tl;dr
   :class: tip

   Chapter 17 enumerates MPI-1 bindings, datatypes, constants, a callback typedef, and the C++ bindings that were REMOVED in MPI-3.0. Per §17.1.1 these are explicitly NOT required: an implementation MAY provide them for backwards compatibility but need not. A default Open MPI build on this checkout is conformant by OMISSION: with no --enable-mpi1-compatibility (default OFF) and any modern C11/C++11 compiler, OMPI_OMIT_MPI1_COMPAT_DECLS=1 and the removed C names are #if'd out of mpi.h and redefined to a compile-time _Static_assert/static_assert 'was removed in MPI-3.0' diagnostic naming the MPI-2/3 replacement (mpi.h.in:332-368, 4080-4170). The framework's CONDITIONAL class is reserved for REQUIRED behavior reachable only via a non-default path; because removed interfaces are NOT required, the gated --enable-mpi1-compatibility exposure is an OPTIONAL back-compat extension, and the conformant state is the DEFAULT -> these topics are SUPPORTED (conformance-by-omission), with --enable-mpi1-compatibility recorded as a gate note for the optional restoration. This applies UNIFORMLY to the removed functions, MPI_LB/MPI_UB, MPI_COMBINER\_\*_INTEGER constants, MPI_Handler_function, and C++ bindings; b2's residual CONDITIONAL label on the COMBINER constants is corrected to SUPPORTED for the same reason. The C/Fortran symbol bodies are always compiled into libmpi for ABI; only header visibility is gated. One asymmetry (verified): the Fortran-side removed names MPI_LB/MPI_UB and MPI_COMBINER\_\*_INTEGER are defined UNCONDITIONALLY in mpif-handles.h/mpif-constants.h regardless of the flag -- conformant (the standard forbids requiring removed names, not defining extra ones). b1 and b2 cover this chapter; b3 merged ch16+17 and its flagged_symbols (in the ch16 bundle, all not_applicable -> SUPPORTED-by-omission) are b3's verdict for the removed-function topics here.

Conformance summary
-------------------


**8** reconciled requirement(s)/behavior(s): 8 supported, 0 conditional, 0 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (100.0% of applicable fully supported).

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Removed MPI-1 C functions (MPI_Address, MPI_Errhandler_create/get/set, MPI_Type_extent/lb/ub/hindexed/hvector/struct) are correctly treated as removed by default: prototypes suppressed and any use yields a compile-time diagnostic naming the MPI-2 replacement; standard does not require them
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - When --enable-mpi1-compatibility IS set, the removed C functions are usable and behave as their historical MPI-1 definitions; the symbol bodies are compiled into libmpi unconditionally for ABI regardless of the flag
     - b1=COND/b2=SUPP/b3=SUPP (conflict)
   * - Removed datatypes MPI_LB and MPI_UB are correctly treated as removed by default (replacement MPI_Type_create_resized present and unconditional); MPI_LB/MPI_UB user macros gated off, redefined to a static-assert naming the replacement
     - b1=SUPP/b2=SUPP/b3=NA (unanimous)
   * - Removed constants MPI_COMBINER_HINDEXED_INTEGER / HVECTOR_INTEGER / STRUCT_INTEGER (no replacement) are correctly treated as removed by default; internal combiner recognition stays unconditional so Type_get_envelope/get_contents introspection of legacy-described types still works
     - b1=SUPP/b2=COND/b3=NA (conflict)
   * - Removed callback typedef MPI_Handler_function (replacement MPI_Comm_errhandler_function) is correctly absent by default; replacement present unconditionally
     - b1=sile/b2=SUPP/b3=SUPP (unanimous)
   * - Fortran-side removed names (MPI_LB, MPI_UB, MPI_COMBINER\_\*_INTEGER) are defined UNCONDITIONALLY in the Fortran headers regardless of --enable-mpi1-compatibility (the one place removed symbols are present by default) -- conformant because the standard forbids requiring removed names, not defining extra ones
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - C++ bindings were removed in MPI-3.0; the MPI namespace is reserved. Open MPI provides no C++ class bindings, trivially satisfying the removal
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - mpi_f08 (an MPI-3+ construct) correctly contains NONE of the MPI-3.0-removed functions
     - b1=sile/b2=SUPP/b3=SUPP (unanimous)
