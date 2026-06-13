.. _release-notes-mpi-5.0-ch16:

Deprecated Interfaces
=====================


.. admonition:: tl;dr
   :class: tip

   Open MPI fully implements MPI-5.0 Chapter 16 (Deprecated Interfaces) by default on this checkout (HEAD 71d6dcf7f4). Deprecation != removal: every deprecated procedure, typedef, predefined function constant, and attribute key the chapter enumerates is present and SUPPORTED in a default build. The deprecated prototypes are declared unconditionally in ompi/include/mpi.h.in (the block beginning at line 3935), annotated with __mpi_interface_deprecated\_\_ pointing at the modern replacement, and sit ABOVE the OMPI_OMIT_MPI1_COMPAT_DECLS gate at line 4080 that guards only the Chapter 17 removed symbols. b1 and b2 agree across the board (all SUPPORTED); b3 merged ch16+17 and left its behaviors array empty, addressing only a few non-procedure deprecations (typedefs, MPI_HOST, MPI_Sizeof) in caveats and (mis)labeling MPI_Sizeof config_gated. Per the framework's pre-resolved Fortran rule, MPI_Sizeof and the f08/mpif.h bindings are default-on and only removable via the optional --disable-mpi-fortran flag, so they are SUPPORTED with a gate note, not CONDITIONAL. The C-only (no-f08) absence for the attr/keyval routines and predefined function constants is exactly what the standard prescribes ('an mpi_f08 interface was never defined'), so it is conformant, not a gap. Neither in-flight item (ABI PR #13280, ompio-info #13367) touches deprecated interfaces.

Conformance summary
-------------------


**11** reconciled requirement(s)/behavior(s): 11 supported, 0 conditional, 0 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (100.0% of applicable fully supported).

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Deprecated attribute/keyval routines (MPI_Attr_get/put/delete, MPI_Keyval_create/free) remain available and route through the same attribute engine as MPI_Comm\_\*_attr / MPI_Comm\_\*_keyval replacements
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Predefined attribute-copy/delete function constants behave per standard: MPI_NULL_COPY_FN returns flag=0/SUCCESS; MPI_DUP_FN sets flag=1 and copies in->out, returns SUCCESS; MPI_NULL_DELETE_FN returns SUCCESS
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Deprecated typedefs remain defined: MPI_Copy_function, MPI_Delete_function (used by MPI_Keyval_create), and the errhandler aliases MPI_Comm_errhandler_fn / MPI_File_errhandler_fn / MPI_Win_errhandler_fn (equivalent to the renamed \*_function types)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Cancelling a send via MPI_Cancel remains callable but is deprecated; the implementation accepts the call and may let the send complete normally (cancel-or-complete is permitted), with a tunable deprecation warning
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_get and MPI_Info_get_valuelen (deprecated in MPI-4.0, superseded by MPI_Info_get_string) remain present in C and f08 with full semantics (flag set, truncation, erroneous if key > MPI_MAX_INFO_KEY)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Deprecated MPI_T_ERR_INVALID_ITEM constant is provided alongside its replacement MPI_T_ERR_INVALID_INDEX (distinct integer values)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Sizeof Fortran routine (deprecated in MPI-4.1, superseded by storage_size()/c_sizeof()) remains available; returns base-element byte size; Fortran-only by design (no C binding)
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - Deprecated MPI\_\*_X large-count routines (MPI_Type_size_x, Type_get_extent_x, Type_get_true_extent_x, Get_elements_x, Status_set_elements_x; deprecated in MPI-4.1, superseded by _c bindings) behave per their canonical descriptions; present in C and f08
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_HOST predefined attribute on MPI_COMM_WORLD returns the host process rank or MPI_PROC_NULL if no host, identically on all ranks (deprecated since MPI-4.1)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - mpif.h include file remains available though its use is deprecated (narrative-only deprecation; file still shipped/installed)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Chapter 16 deprecated interfaces are available in a DEFAULT build (NOT gated behind --enable-mpi1-compatibility, unlike Chapter 17 removed interfaces)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
