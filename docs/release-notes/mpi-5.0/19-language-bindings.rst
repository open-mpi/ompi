.. _release-notes-mpi-5.0-ch19:

Language Bindings
=================


.. admonition:: tl;dr
   :class: tip

   Open MPI's main branch conforms strongly to MPI-5.0 Chapter 19. All C language-binding machinery is present and unconditional: MPI_Aint/Count/Offset/Fint types, the full set of MPI\_<Obj>_f2c/c2f handle converters (14 object classes), all six MPI_Status conversion routines (f2c/c2f/f082c/c2f08/f2f08/f082f), the C-side Fortran-status constants (MPI_F_STATUS_SIZE, zero-based MPI_F_SOURCE/TAG/ERROR, extern MPI_F_STATUS(ES)_IGNORE / MPI_F08_STATUS(ES)_IGNORE sentinels), large-count _c procedures and callbacks, and MPI_Aint_add/diff. All three Fortran support methods (mpi_f08 with BIND(C) %MPI_VAL derived-type handles, the mpi module, and deprecated mpif.h) are built by default whenever a Fortran compiler is present; --disable-mpi-fortran removes them (gate note only, still SUPPORTED). The headline correction across reviewers: MPI_SUBARRAYS_SUPPORTED and MPI_ASYNC_PROTECTS_NONBLOCKING are hard-coded .false. in mpif-config.h.in unconditionally -- the configure variable that would set .true. under TS-29113 reaches only Makefiles, never any Fortran source -- so the exposed values are .false. ALWAYS (not '.true. when TS detected'). That .false. is a standard-permitted honest under-claim, hence SUPPORTED. Genuinely below-SUPPORTED: extended Fortran KIND datatypes (MPI_REAL16/COMPLEX32/INTEGER16 ...) are CONDITIONAL on compiler representation support; the 6 MPI_Status\_{get,set}_{source,tag,error} f08 bindings are genuinely UNSUPPORTED (C bindings present, no \*_f08.F90); and the f08 standard-ABI compiler wrapper / ABI Fortran bindings are MISSING_INFLIGHT (PR #13280, really Chapter 20 surface).

Conformance summary
-------------------


**20** reconciled requirement(s)/behavior(s): 17 supported, 1 conditional, 0 partial, 1 unsupported, 1 missing-but-in-flight, 0 N/A (85.0% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


mpi_f08 bindings for the 6 status accessors MPI_Status\_{get,set}_{source,tag,error}
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Unsupported**
:Standard: §19.3.5 / status accessors (MPI-4.0)
:Reviewers: b1=silent, b2=silent, b3=UNSUPPORTED (raised by 1 reviewer)
:Evidence: C bindings present: ompi/mpi/c/status_get_source_generated.c, status_set_source_generated.c (and tag/error). NO f08 binding: ompi/mpi/fortran/use-mpi-f08/ contains status_set_cancelled_f08.F90 and status_set_elements_x_f08.F90 but NO status\_{get,set}_{source,tag,error}_f08.F90 (verified by directory listing)
:Notes: Unique to b3 (it filed it as an awareness caveat since these procedures' home chapter is point-to-point, but the task instructs surfacing it here). Pre-resolved fact confirms the gap is genuine and NOT in-flight. The MPI-4.0/5.0 standard defines f08 bindings for these; their absence is a real f08-binding gap => UNSUPPORTED. C bindings exist and work.

f08 standard-ABI compiler wrapper (mpifort_abi) and ABI Fortran bindings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20 / p.853 (ABI chapter surface)
:Reviewers: b1=UNSUPPORTED, b2=silent, b3=silent (raised by 1 reviewer)
:Evidence: ``docs/release-notes/mpi-5.0.rst:115 (documents absence); in-flight via PR #13280 (https://github.com/open-mpi/ompi/pull/13280)``
:Notes: Unique to b1, marked UNSUPPORTED. Flipped per the authoritative in-flight tiebreaker: ALL ABI surface (Chapter 20, including the standard-ABI Fortran wrapper) is MISSING_INFLIGHT via PR #13280, never UNSUPPORTED. This is really Chapter 20 surface listed here only because b1 raised it.

Size-specific / extended Fortran KIND datatypes: MPI_REAL16, MPI_COMPLEX32, MPI_INTEGER16, MPI_LOGICAL1..16 etc. usable for communication
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §19.1.9 / p.804
:Reviewers: b1=CONDITIONAL, b2=silent, b3=silent (raised by 1 reviewer)
:Gate: Fortran compiler must provide each specific representation (e.g. OMPI_HAVE_FORTRAN_REAL16); unsupported representations initialize to OMPI_DATATYPE_INIT_UNAVAILABLE
:Evidence: ompi/datatype/ompi_datatype_module.c:168-234 (e.g. ompi_mpi_complex32, ompi_mpi_logical1..8 initialized via OMPI_DATATYPE_INIT_UNAVAILABLE macro by default, populated only when the compiler provides the representation)
:Notes: Unique to b1, status correct. Confirmed by pre-resolved fact: extended KINDs are CONDITIONAL on compiler representation support. Symbols always present but may be unavailable. This is the one genuinely CONDITIONAL topic in the chapter.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - C handle conversion MPI\_<Obj>_f2c / _c2f for all opaque-object types (comm, datatype, errhandler, file, group, info, message, op, request, session, win), null->null / invalid->invalid
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - C status conversion: all six routines MPI_Status_f2c/c2f/f082c/c2f08/f2f08/f082f preserving hidden count/elements/cancellation info, with MPI_STATUS_IGNORE handling
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Status_f2f08 / MPI_Status_f082f callable from both the mpi_f08 and the mpi modules (excluded only from deprecated mpif.h, as the standard requires)
     - b1=COND/b2=SUPP/b3=sile (conflict)
   * - C-side Fortran status constants: MPI_F_STATUS_SIZE, zero-based indices MPI_F_SOURCE/TAG/ERROR (one less than Fortran), and extern sentinels MPI_F_STATUS(ES)_IGNORE / MPI_F08_STATUS(ES)_IGNORE
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - mpi_f08 module: BIND(C) derived-type handles (TYPE(MPI_Comm) etc.) with single integer MPI_VAL component, TYPE(MPI_Status) with MPI_SOURCE/TAG/ERROR, .EQ./.NE. handle-comparison operator overloads, ISO_C_BINDING usage
     - b1=COND/b2=SUPP/b3=sile (conflict)
   * - All three Fortran support methods provided: USE mpi_f08, USE mpi, INCLUDE mpif.h (implementation must provide mpi_f08 and/or the mpi+mpif.h pair); built by default when a Fortran compiler is found
     - b1=COND/b2=SUPP/b3=SUPP (conflict)
   * - MPI_SUBARRAYS_SUPPORTED and MPI_ASYNC_PROTECTS_NONBLOCKING compile-time LOGICAL constants set per Fortran support method without over-claiming
     - b1=COND/b2=SUPP/b3=SUPP (conflict)
   * - f08 nonblocking/persistent choice-buffer bindings provided (assumed-type ignore-TKR TYPE(\*),DIMENSION(\*) + ASYNCHRONOUS attribute scheme; _f08ts specific procedures generated)
     - b1=COND/b2=sile/b3=SUPP (conflict)
   * - MPI_F_SYNC_REG no-op register/memory-synchronization helper (Fortran-only), implemented so the compiler cannot optimize away the empty body
     - b1=COND/b2=SUPP/b3=SUPP (conflict)
   * - MPI_Sizeof (deprecated, Fortran-only) generic numeric-size subroutine
     - b1=sile/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Type_create_f90\_{real,complex,integer} return matching predefined unnamed datatypes (by p,r); erroneous/MPI_ERR_ARG for unsupported (p,r); MPI_Type_match_size switches on MPI_TYPECLASS\_\*
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - F90-created datatypes are predefined (un-freeable, need not be committed), same handle returned for same (XXX,p,r) via hash cache, MPI_TYPE_GET_ENVELOPE returns MPI_COMBINER_F90\_\* exposing p,r; external32 representation for F90 types incl. 16-byte double-extended
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Large-count C procedures: _c-suffixed routines with MPI_Count/MPI_Aint/MPI_Offset count and displacement params; large-count callbacks (MPI_User_function_c, MPI_Datarep_conversion_function_c) and MPI_CONVERSION_FN_NULL_C; MPI_Op_create_c / MPI_Register_datarep_c directly callable
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Fortran large-count via mpi_f08 interface polymorphism (specific _c procedure with KIND=MPI_COUNT_KIND/ADDRESS_KIND/OFFSET_KIND); _c interfaces correctly absent from use mpi and mpif.h
     - b1=COND/b2=SUPP/b3=sile (conflict)
   * - PMPI name-shift (profiling interface) for every routine in C and for Fortran (f08 + mpif.h) specific procedure names
     - b1=COND/b2=sile/b3=sile (unique)
   * - win_f2c implemented as a plain handwritten file rather than a generated .c.in (cosmetic naming difference)
     - b1=sile/b2=sile/b3=SUPP (unique)
   * - Interlanguage interoperability: init/finalize from any language affects all; opaque objects, MPI_Get_address, info contents, predefined reduce ops behave identically across languages; relaxed interlanguage type matching
     - b1=sile/b2=SUPP/b3=sile (unique)
