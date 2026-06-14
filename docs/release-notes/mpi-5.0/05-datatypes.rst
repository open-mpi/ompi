.. _release-notes-mpi-5.0-ch05:

Datatypes
=========


.. admonition:: tl;dr
   :class: tip

   Open MPI's Chapter 5 (Datatypes) is overwhelmingly SUPPORTED by default: the full derived-datatype constructor family (contiguous/vector/hvector/indexed/hindexed/indexed_block/hindexed_block/struct/subarray/darray/resized) and their large-count _c variants, address arithmetic (Get_address, Aint_add/diff), extent/true-extent (incl _x), commit/free/dup, Get_elements/_x, full combiner decoding via Type_get_envelope/get_contents, and canonical external32 pack/unpack are all delivered unconditionally by the always-built opal/datatype + ompi/datatype engine. Three genuine, narrow deviations exist: (1) the large-count MPI_Pack_c/MPI_Unpack_c/MPI_Pack_size_c variants truncate their 64-bit size through (unsigned int)/(int) casts, so the >2-4 GiB case the _c API exists to serve is mishandled (PARTIAL); (2) MPI_Type_get_value_index only returns the predefined NAMED pair types and yields MPI_DATATYPE_NULL for any other value/index pair instead of constructing an unnamed datatype, so the MPI_COMBINER_VALUE_INDEX combiner is unreachable (PARTIAL). The optional Fortran KIND datatypes (INTEGER16, REAL16, COMPLEX32, etc.) and the MPI_TYPE_CREATE_F90\_\* achievable precision set are CONDITIONAL on configure-time Fortran-compiler KIND autodetection (standard-permitted optionality). The base int-typed Pack/Unpack/Pack_size and everything else are conformant.

Conformance summary
-------------------


**22** reconciled requirement(s)/behavior(s): 18 supported, 2 conditional, 2 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (81.8% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


Big-count MPI_Pack_c / MPI_Unpack_c / MPI_Pack_size_c (>2-4 GiB size handling)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §5.2 / p.176-180 (MPI-4.0 large-count)
:Reviewers: b1=SUPPORTED, b2=SUPPORTED, b3=PARTIAL (reviewers disagreed)
:Evidence: ompi/mpi/c/pack.c.in:93 -> pack_generated.c:134,188 (MPI_Pack_c has MPI_Count outsize but truncation check casts '(unsigned int)outsize'); ompi/mpi/c/unpack.c.in:100 -> unpack_generated.c:138,198 (MPI_Unpack_c, MPI_Count insize, '(unsigned int)insize'); ompi/mpi/c/pack_size.c.in:63 -> pack_size_generated.c:84,112 (MPI_Pack_size_c has MPI_Count\* size but '\*size = (int)length'). Correct pattern at ompi/datatype/ompi_datatype_external.c:56 uses (size_t).
:Notes: CONFLICT resolved in favor of b3 (verified in generated source). The shared PROTOTYPE COUNT template is correct for the int-typed base bindings but, for the _c bindings where outsize/insize are 64-bit MPI_Count and size is MPI_Count\*, the truncation check casts through (unsigned int) and pack_size stores via (int). Sizes/positions above ~2-4 GiB - exactly the case the _c API exists to support - yield spurious or missed MPI_ERR_TRUNCATE and a truncated pack size. b1 and b2 assessed the family at the API-presence level (symbols present, position semantics OK) and missed the _c truncation. Narrow but genuine non-conformance => PARTIAL.

MPI_Type_get_value_index / MPI_COMBINER_VALUE_INDEX (MPI-5.0)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §5.1 value-index, §5.1.13 combiner table / p.161 (MPI-5.0)
:Reviewers: b1=PARTIAL, b2=silent, b3=SUPPORTED (reviewers disagreed)
:Evidence: ompi/datatype/ompi_datatype_create.c:128-180 (ompi_datatype_get_value_index returns predefined NAMED pair types only; any other value/index pair => MPI_DATATYPE_NULL; code comment 'not a complete implementation ... doesn't support possible unnamed datatypes'); ompi/mpi/c/type_get_value_index.c.in (binding present); MPI_COMBINER_VALUE_INDEX defined ompi/include/mpi.h.in:836 but NO VALUE_INDEX decode case in ompi_datatype_args.c and no constructor producing it
:Notes: CONFLICT resolved to PARTIAL. The MPI_Type_get_value_index symbol/binding exists and works for the standard predefined pair types (FLOAT_INT, DOUBLE_INT, 2INT, LONG_INT, etc.), but for any other value_type/index_type pair it returns MPI_DATATYPE_NULL instead of constructing an unnamed pair datatype, and the code self-documents this as incomplete. Consequently MPI_COMBINER_VALUE_INDEX is unreachable (defined as a constant only, no decode/construct path). b3 marked it SUPPORTED only on the basis that the constant '=19' is defined - that conflates symbol/constant presence with behavioral completeness. b1's PARTIAL is correct. (Merged b1's two findings - get_value_index PARTIAL and the VALUE_INDEX combiner UNSUPPORTED - into one topic, since they are one underlying gap: the combiner is unreachable precisely because no unnamed pair type is ever constructed. PARTIAL rather than UNSUPPORTED because the predefined-pair path works.)

MPI_TYPE_CREATE_F90_REAL/_COMPLEX/_INTEGER (precision/range mapping, cached consistent handles)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §5.1 F90 ctors / p.161
:Reviewers: b1=CONDITIONAL, b2=SUPPORTED, b3=silent (reviewers disagreed)
:Gate: Fortran build enabled (--enable-mpi-fortran, default on) AND the requested precision/range is achievable by a configure-detected Fortran KIND (e.g. OMPI_HAVE_FORTRAN_REAL2); MPI_DATATYPE_NULL returned when unsatisfiable
:Evidence: ``ompi/mpi/c/type_create_f90_real.c.in:57-113 (MPI_UNDEFINED/null handling + opal_hash_table per-(p,r) cache); type_create_f90_{complex,integer}.c.in``
:Notes: CONFLICT. The mapping logic and per-(p,r) caching (consistency requirement) are fully implemented and built unconditionally in C (b2's point). However the \*achievable set of precisions\* depends on which Fortran KIND types configure detected, so the function may return MPI_DATATYPE_NULL for precisions the compiler cannot provide (b1's point). Marking CONDITIONAL to capture that the practically-reachable precision range is gated on configure-time Fortran KIND autodetect; the procedure itself is present and correct.

Optional Fortran KIND datatypes (MPI_INTEGER1/2/4/8/16, MPI_REAL2/4/8/16, MPI_COMPLEX4/8/16/32, MPI_LOGICAL1..16)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Conditional**
:Standard: §5.1 optional datatypes
:Reviewers: b1=silent, b2=CONDITIONAL, b3=silent (raised by 1 reviewer)
:Gate: Per-KIND configure-time Fortran-compiler autodetect (OMPI_HAVE_FORTRAN\_<KIND>); --disable-mpi-fortran removes Fortran exposure
:Evidence: ompi/datatype/ompi_datatype_module.c:216-286 (per-KIND #if OMPI_HAVE_FORTRAN\_\* ... #else OMPI_DATATYPE_INIT_UNAVAILABLE); config/ompi_fortran_check.m4:52,74-78,156-158,180-182 (detection gated, size/KIND-value mismatch disables a KIND); ompi/include/mpi.h.in:1356-1395 (handles declared)
:Notes: Unique (b2), CONDITIONAL confirmed. The standard marks these datatypes optional; OMPI populates each only when configure autodetects the Fortran compiler actually supports that KIND, else the handle maps to UNAVAILABLE. Genuinely per-KIND configure-dependent. Not in-flight (PR #13280 / ompio-info touch other chapters).

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Derived datatype constructors (contiguous/vector/hvector/indexed/hindexed/indexed_block/hindexed_block/struct) incl big-count _c forms
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Subarray constructor (n-D subarray with lb/ub markers, correct extent)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Distributed-array (DARRAY) constructor with BLOCK/CYCLIC/NONE distributions and ORDER_C/FORTRAN
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Type_create_resized + lb/ub-marker (resized) extent semantics
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Address/size queries: MPI_Get_address, MPI_Aint_add, MPI_Aint_diff (portable address arithmetic)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Type_size / _c return type size; MPI_UNDEFINED when value does not fit OUT param
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Type_commit (idempotent), MPI_Type_free (sets handle to NULL, in-flight comms/derived types unaffected)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Type_dup copies attributes via key copy callbacks; preserves committed state/bounds
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - General datatypes usable in send/recv with count>1 (concatenation/type-signature matching), MPI_BOTTOM + absolute-address buffers
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Get_elements / _c / _x return basic element count, MPI_UNDEFINED on non-integral or >INT_MAX
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Datatype decoding: MPI_Type_get_envelope (combiner + arg counts), MPI_Type_get_contents (reconstruct ctor args); predefined => MPI_COMBINER_NAMED; deprecated \*_INTEGER aliases decoded
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Big-count decode: non-_c get_envelope/get_contents raise MPI_ERR_TYPE when num_large_counts>0; _c variants return large-count arrays
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Pack / MPI_Unpack / MPI_Pack_size (incremental position, MPI_PACKED relaxed matching) - base int-typed variants
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Canonical external32 pack/unpack (MPI_Pack_external / Unpack_external / Pack_external_size) in big-endian IEEE external32 datarep
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_TYPE_MATCH_SIZE returns matching named type for (typeclass,size)
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Fortran-2008 bindings for all Chapter 5 procedures/constants
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Datatype error classes (MPI_ERR_TYPE, MPI_ERR_ARG, MPI_ERR_COUNT) for invalid datatype/argument usage
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - MPI_LB / MPI_UB deprecation and \*_INTEGER deprecated combiner back-compat
     - b1=sile/b2=SUPP/b3=SUPP (unanimous)
