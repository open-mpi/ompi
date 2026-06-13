.. _release-notes-mpi-5.0-ch07:

Groups, Contexts, Communicators, and Caching
============================================


.. admonition:: tl;dr
   :class: tip

   Open MPI's main checkout (HEAD 71d6dcf7f4) conforms strongly to MPI-5.0 Chapter 7. All chapter procedures are present in C and mpi_f08, group constructors/accessors, communicator dup/idup/create/split/free, inter-communicator ops, the session-model create-from-group / intercomm-create-from-groups constructors, the full attribute-caching engine (comm/win/type keyvals, copy/delete callbacks, predefined callbacks and predefined attributes), object naming, and comm set_info/get_info round-trip are all SUPPORTED by default. MPI_Comm_split_type SHARED/HW_GUIDED/HW_UNGUIDED/RESOURCE_GUIDED are implemented in libmpi using opal's bundled hwloc (default-built) and PMIX_LOCALITY locality, so they are SUPPORTED (not CONDITIONAL); b3's optional_lib_gated downgrade is rejected because hwloc is bundled and the graceful MPI_COMM_NULL/MPI_COMM_SELF degradation is itself spec-conformant. The session-model create-from-group constructors are pure libmpi (operate on an already-resolved group) so they are SUPPORTED with a runtime gate note for pset resolution (not CONDITIONAL). The single real gap is the mpi_pset_name sub-path of MPI_COMM_TYPE_RESOURCE_GUIDED (a normative MPI-5.0 addition, verified in mpi50-report.pdf p.331-332): the key is recognized at the binding layer but never resolved to a split, so a pset-only call yields MPI_COMM_NULL. Because returning MPI_COMM_NULL when the implementation does not recognize/support the mpi_pset_name value is itself explicitly spec-sanctioned (mpi50-report.pdf p.332), this is PARTIAL (present-but-incomplete, conformant-but-degraded), not UNSUPPORTED.

Conformance summary
-------------------


**36** reconciled requirement(s)/behavior(s): 35 supported, 0 conditional, 1 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (97.2% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI_Comm_split_type MPI_COMM_TYPE_RESOURCE_GUIDED: mpi_pset_name sub-path not resolved to a split (yields MPI_COMM_NULL); mpi_hw_resource_type sub-path works
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §7.4.2 / p.331-333
:Reviewers: b1=PARTIAL, b2=UNSUPPORTED, b3=SUPPORTED (reviewers disagreed)
:Evidence: ompi/mpi/c/comm_split_type.c.in:118-131 (binding accepts mpi_pset_name to pass the gate when flag_res set); ompi/communicator/comm.c:1471-1481 (ompi_comm_split_type reads ONLY mpi_hw_resource_type via opal_info_get; flag==0 when pset-only -> MPI_COMM_NULL at :1478-1480). Negative evidence: mpi_pset_name appears only in comm_split_type.c.in/_generated.c, nowhere in comm.c or any MCA component. Standard normative text verified directly: MPI-5.0 (for-claude/mpi50-report.pdf p.331-332) defines MPI_COMM_TYPE_RESOURCE_GUIDED with the 'mpi_pset_name' info key (logical resource / process-set), and explicitly lists 'MPI_COMM_NULL is also returned ... The MPI implementation does not recognize the value associated with the info key "mpi_hw_resource_type" or "mpi_pset_name"' as a sanctioned response.
:Notes: 3-way conflict. b3 generically marks split_type SUPPORTED; b1 PARTIAL; b2 missing_gap (UNSUPPORTED). Verified the requirement against the standard itself (not just reviewer claims): MPI_COMM_TYPE_RESOURCE_GUIDED + the mpi_pset_name info key ARE normative MPI-5.0 (mpi50-report.pdf p.331-332; added in MPI-5.0 per the change-log), NOT an OMPI extension -- so there is a real required behavior, ruling out SUPPORTED/NA for this sub-path. Resolved to PARTIAL: the mpi_hw_resource_type RESOURCE_GUIDED path is fully implemented, AND the mpi_pset_name key IS recognized at the binding layer (present) but never wired through to a pset-based split (incomplete) -> present-but-incomplete = PARTIAL. b2's UNSUPPORTED is rejected: the standard explicitly sanctions returning MPI_COMM_NULL when the implementation does not recognize/support the mpi_pset_name value (mpi50-report.pdf p.332 bullet list), so OMPI's MPI_COMM_NULL is conformant-but-degraded, not 'genuinely required behavior absent.' The pset-to-group machinery exists (ompi_group_from_pset, instance.c:1424) but is not wired into ompi_comm_split_type. Not covered by any in-flight PR (PR #13280 = ABI/Ch.20; ompio-info branch = File/IO).

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Group accessors (size/rank/compare/translate_ranks; MPI_PROC_NULL maps to MPI_PROC_NULL)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Set-like group constructors (union/intersection/difference) honor ordering/associativity; may yield MPI_GROUP_EMPTY
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Group_incl/excl and range incl/excl (scalable strided representation per advice-to-implementors)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Group_from_session_pset creates a group from a session/pset name (incl mpi://world, mpi://self); unknown pset -> MPI_ERR_ARG
     - b1=COND/b2=SUPP/b3=COND (conflict)
   * - MPI_Group_free reference-counts, marks for deallocation, sets handle to MPI_GROUP_NULL
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Communicator accessors size/rank/compare with inter-communicator local-group semantics (Table 7.1)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Comm_dup / dup_with_info copy group/topology/error handler/cached attributes (copy callbacks), new context
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Comm_idup / idup_with_info nonblocking dup returning a request (snapshot-at-call)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Comm_create (intra + inter, disjoint subgroups; MPI_COMM_NULL for non-members)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Comm_create_group (subgroup-collective, intra-communicator only)
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - MPI_Comm_create_from_group from group + stringtag + info + errhandler (stringtag <= MPI_MAX_STRINGTAG_LEN >= 63); sets MPI_TAG_UB keyval
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - MPI_Comm_split by color/key with tie-break by old rank; MPI_UNDEFINED -> MPI_COMM_NULL; works on inter-comms
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - MPI_Comm_split_type MPI_COMM_TYPE_SHARED groups shared-memory domain
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Comm_split_type MPI_COMM_TYPE_HW_GUIDED via mpi_hw_resource_type; MPI_COMM_NULL on missing/unknown value or MPI_INFO_NULL
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - MPI_Comm_split_type MPI_COMM_TYPE_HW_UNGUIDED produces strict subset by iterating topology classes; MPI_COMM_NULL when no finer split
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - mpi_hw_resource_type value form: bare keys only; strongly-recommended hwloc:// URI form not matched
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - MPI_Comm_free collective deallocation; handle -> MPI_COMM_NULL; delete callbacks for cached attributes invoked
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Communicator info round-trip: MPI_Comm_set_info / MPI_Comm_get_info return recognized + default hints in a fresh info object
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Recognized communicator info assert hints (no_any_source/no_any_tag/allow_overtaking/exact_length/memory_alloc_kinds) honored with defaults
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - mpi_assert_strict_persistent_collective_ordering hint silently ignored (conformant)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - MPI_Comm_test_inter / remote_size / remote_group inter-communicator introspection
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - MPI_Intercomm_create binds two intra-communicators via peer comm + leaders + tag
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - MPI_Intercomm_create_from_groups from disjoint local/remote groups + stringtags (session model)
     - b1=SUPP/b2=SUPP/b3=COND (conflict)
   * - MPI_Intercomm_merge merges local+remote into intra-communicator ordered by high flag
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Attribute caching on communicators: create/free keyval, set/get/delete attr, copy-on-dup + delete-on-free callbacks (synchronous, cross-language)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Keyval created for one object kind rejected when used with a different kind (type-checking)
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - Predefined attribute callbacks (COMM/WIN/TYPE NULL_COPY_FN, DUP_FN, NULL_DELETE_FN) behave per spec; deprecated MPI_Copy_function/MPI_Delete_function (C-only)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Attribute caching on windows (keyval/set/get/delete attr)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Attribute caching on datatypes (keyval/set/get/delete attr)
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Attribute copy/delete callback prototypes available as f08 abstract interfaces (COMM/WIN/TYPE)
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Predefined communicator attributes MPI_TAG_UB / MPI_HOST / MPI_IO / MPI_WTIME_IS_GLOBAL / MPI_UNIVERSE_SIZE / MPI_LASTUSEDCODE populated on MPI_COMM_WORLD
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - MPI_ERR_KEYVAL error class for invalid keyvals
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Object naming set/get for comm/type/win; MPI_MAX_OBJECT_NAME (>=64, OMPI=128) truncation; empty string when unset
     - b1=SUPP/b2=SUPP/b3=sile (partial-overlap)
   * - Predefined communicator default names (MPI_COMM_WORLD/SELF/PARENT/NULL) and datatype/window default names
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - All Chapter 7 procedures present in both C and mpi_f08 bindings (with PMPI\_ duals)
     - b1=sile/b2=SUPP/b3=sile (unique)
