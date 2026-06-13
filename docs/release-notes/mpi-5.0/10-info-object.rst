.. _release-notes-mpi-5.0-ch10:

The Info Object
===============


.. admonition:: tl;dr
   :class: tip

   Open MPI fully and unconditionally supports MPI-5.0 Chapter 10 in this main checkout. All nine info procedures (MPI_Info_create, _create_env, _delete, _dup, _free, _get_nkeys, _get_nthkey, _get_string, _set) plus the two deprecated-but-retained queries (MPI_Info_get, MPI_Info_get_valuelen) are present in C, and all nine f08 procedures are present (default-on Fortran build). Constants MPI_MAX_INFO_KEY (36, in [33,256]), MPI_MAX_INFO_VAL (256), MPI_INFO_NULL, MPI_INFO_ENV are defined. The implementation lives in the always-compiled opal/util/info.c and ompi/info/info.c; every chapter requirement is default-SUPPORTED with no conditional, in-flight, or genuine-gap item. MPI_INFO_ENV is populated with the standard keys it can determine (command, argv, maxprocs, soft, host, arch, wdir, thread_level, mpi_initial_errhandler); the 'file' key is legitimately absent because Open MPI does not launch via the appfile mechanism the standard ties that key to, which is conformant per the standard's per-key availability wording. Reviewers were unanimous on substance; the only normalization issues were b2's self-corrected f08 caveat (adjusted to default-supported) and the b3-only deprecated-symbol note (NA, correctly).

Conformance summary
-------------------


**24** reconciled requirement(s)/behavior(s): 23 supported, 0 conditional, 0 partial, 0 unsupported, 0 missing-but-in-flight, 1 N/A (100.0% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


Deprecated MPI_Info_get / MPI_Info_get_valuelen (removed from MPI-4.1 standard body) retained as working back-compat bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **N/A**
:Standard: MPI-4.0 deprecation; removed in MPI-4.1 / absent from MPI-5.0 §10 body
:Reviewers: b1=silent, b2=silent, b3=NA (raised by 1 reviewer)
:Evidence: ompi/mpi/c/info_get.c.in:57-100 (validates handle/valuelen/key/value/flag, copies via opal_string_copy(value,...,valuelen+1)); ompi/mpi/c/info_get_valuelen.c.in:54-86 (-> ompi_info_get_valuelen, opal/util/info.c:325)
:Notes: Unique to b3; correctly NA. Neither routine is part of the MPI-5.0 Chapter 10 procedure set (MPI_Info_get_string replaces them), so they impose no MPI-5.0 requirement. Open MPI retains correct, functional bindings for backward compatibility -- not required, hence NA, not a gap.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Info object stores an unordered set of (key,value) string pairs; a key maps to exactly one value (set overrides existing)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Info caches arbitrary (key,value) pairs with no key whitelist; key recognition is not required for storage
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - get_nkeys / get_nthkey / get_string retain all (key,value) pairs (layered-functionality cache)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Key/value max length implementation-defined: MPI_MAX_INFO_KEY in [33,256], MPI_MAX_INFO_VAL defined
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_create creates a new empty info object
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - MPI_Info_set adds/overrides a pair and raises MPI_ERR_INFO_KEY / MPI_ERR_INFO_VALUE on over-length key/value
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_delete removes a pair and raises MPI_ERR_INFO_NOKEY when key not defined
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_get_string: key present -> flag=true and value copied; absent -> flag=false and value unchanged
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_get_string buflen protocol: buflen==0 returns required size (value+1 for C NUL) without modifying value; too-small buffer truncates+NUL-terminates and sets buflen to required size
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_get_string (and deprecated get/get_valuelen) treat over-MPI_MAX_INFO_KEY key as erroneous
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - MPI_Info_get_nkeys returns the number of currently defined keys
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - MPI_Info_get_nthkey returns the nth key with stable 0..N-1 numbering while info unmodified
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - MPI_Info_dup duplicates info into an independent object with the same (key,value) pairs and same key ordering
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_free frees info and sets the handle to MPI_INFO_NULL
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_create_env produces an info consistent with MPI_INFO_ENV; user frees via MPI_Info_free; ISO C binding accepts argc/argv (0/NULL allowed), Fortran omits them
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Info_create_env is thread-safe and callable before MPI init / after finalize (local); object may be incompletely populated when values unavailable
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_INFO_ENV exposes the §11.2.1 reserved keys (command, argv, maxprocs, soft, host, arch, wdir, file, thread_level)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Info argument is interpreted before the procedure returns; caller may read/modify/free it immediately afterward (all procedures)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Portable value-string representations: booleans 'true'/'false' and decimal integers within a standard integer type
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Both key and value are case sensitive
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Info accessors are thread-safe (per-object serialization)
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Fortran mpi_f08 bindings for the nine standard info procedures
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Fortran leading/trailing space stripping on info key/value (Fortran-binding-only rule)
     - b1=sile/b2=sile/b3=SUPP (unique)
