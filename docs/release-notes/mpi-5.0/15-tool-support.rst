.. _release-notes-mpi-5.0-ch15:

Tool Support
============


.. admonition:: tl;dr
   :class: tip

   Open MPI's main branch substantially conforms to MPI-5.0 Chapter 15. The PMPI profiling interface (15.2) is fully and unconditionally SUPPORTED across all three language bindings (C, mpif-h, use-mpi-f08) via the dual profile/noprofile libtool build emitting weak MPI\_\* over real PMPI\_\* entry points, and MPI_Pcontrol is a conformant no-op. All 52 MPI_T procedures plus 3 callback typedefs are present with C bindings only (the standard defines no Fortran MPI_T bindings, so f08 absence is correct, not a gap). The cvar, pvar, enum, and category sub-interfaces are functionally backed by OPAL's mca_base_var/mca_base_pvar and are SUPPORTED, with two narrow deviations: MPI_T_pvar_readreset is an unimplemented stub (always MPI_T_ERR_INVALID_HANDLE) even though pvars are otherwise live, and MPI_T_category_get_events performs no cat_index validation. The MPI-4.0/5.0 MPI_T events sub-interface (events, event sources, callbacks, dropped-event handling, event read/copy/timestamp/source) is present as default-build symbols but functionally inert: MPI_T_event_get_num and MPI_T_source_get_num always return 0 and every index/handle query returns an error, with no mca_base_event infrastructure anywhere in the tree and no flag/component/library to enable delivery -> PARTIAL (enumeration of an empty space is permitted, but no event is ever delivered). Two MPI-5.0 error constants Table 15.7 mandates, MPI_T_ERR_NOT_ACCESSIBLE and MPI_T_ERR_NOT_SUPPORTED, are genuinely absent from mpi.h.in (real header-level gaps, not in any in-flight PR).

Conformance summary
-------------------


**20** reconciled requirement(s)/behavior(s): 11 supported, 0 conditional, 7 partial, 1 unsupported, 0 missing-but-in-flight, 1 N/A (57.9% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


MPI_T tool-info error constants MPI_T_ERR_NOT_ACCESSIBLE and MPI_T_ERR_NOT_SUPPORTED defined
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Unsupported**
:Standard: §15.3.10 / p.765 (Table 15.7)
:Reviewers: b1=PARTIAL, b2=UNSUPPORTED, b3=UNSUPPORTED (reviewers disagreed)
:Evidence: ompi/include/mpi.h.in MPI_T_ERR block runs MPI_T_ERR_PVAR_NO_ATOMIC=67 (line 752) then MPI_ERR_RMA_RANGE=68 (line 753) - no slot for either constant; tree-wide grep for MPI_T_ERR_NOT_ACCESSIBLE and MPI_T_ERR_NOT_SUPPORTED returns zero hits in \*.c/\*.h/\*.in (excluding 3rd-party); pre-resolved by framework as GENUINELY MISSING
:Notes: Conflict resolved to UNSUPPORTED. Pre-resolved fact: both MPI-5.0 Table 15.7 error constants are genuinely absent from every public Open MPI header. A return-code constant is a preprocessor #define that must exist in an installed header; no configure flag or MCA component can conjure it, so default/conditional are structurally impossible. Not in any in-flight PR (#13280 adds only MPI_ABI_VERSION/SUBVERSION/MPI_ERR_ABI; ompio-info branch adds no constants), so not MISSING_INFLIGHT. b1's PARTIAL undercounts; the constants are simply absent = UNSUPPORTED. Low real-world impact (the standard advises high-QoI implementations avoid returning NOT_ACCESSIBLE, and zero sources means NOT_SUPPORTED is currently unreachable), but the binary definition obligation is unmet.

MPI_T_pvar_readreset atomic read-and-reset
""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §15.3.7 / p.732
:Reviewers: b1=PARTIAL, b2=SUPPORTED, b3=PARTIAL (reviewers disagreed)
:Evidence: ompi/mpi/tool/pvar_readreset.c:33-34 (comment '/\* XXX -- TODO -- Implement me \*/', unconditionally returns MPI_T_ERR_INVALID_HANDLE after the init check); contrast pvar_read.c:42 and pvar_reset.c:42-47 which ARE functional; no mca_base_pvar read-and-reset primitive exists in opal/mca/base
:Notes: Conflict resolved to PARTIAL. b2 lumped readreset into the working pvar interface, but the symbol is an unimplemented stub: it always fails with MPI_T_ERR_INVALID_HANDLE even for a valid started handle, so the standard's atomic read-and-reset is non-functional. b1 and b3 are correct. Because pvars ARE exposed in a default build and a valid handle is allocatable (unlike the events no-op), a conforming call that should read-and-reset atomically instead always fails -> present-but-deviates = PARTIAL. Not UNSUPPORTED because the symbol exists and the rest of the pvar interface works; the read and reset operations are individually available.

MPI_T_category_get_events: invalid cat_index validation
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §15.3.9 / p.760
:Reviewers: b1=silent, b2=silent, b3=PARTIAL (raised by 1 reviewer)
:Evidence: ompi/mpi/tool/category_get_events.c:32-39 (does NO cat_index validation, returns MPI_SUCCESS for any cat_index); contrast sibling category_get_cvars.c:44-48 which maps a bad index to MPI_T_ERR_INVALID_INDEX. Standard defines cat_index range 0..num_cat-1 (for-claude/mpi-standard-5.0-apis.json:46181 desc 'in the range from 0 to num_cat-1')
:Notes: Unique to b3; verified correct. Unlike its siblings, MPI_T_category_get_events accepts any cat_index (including out-of-range) and returns MPI_SUCCESS, where the standard requires MPI_T_ERR_INVALID_INDEX for an invalid category index. The deviation is narrow because no events ever exist so indices[] is never written, but an invalid index that should be rejected is accepted -> PARTIAL (present-but-deviates).

MPI_T event sources: MPI_T_source_get_num / get_info / get_timestamp, ordering, ticks_per_second, max_ticks
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §15.3.8 / p.745
:Reviewers: b1=UNSUPPORTED, b2=SUPPORTED, b3=SUPPORTED (reviewers disagreed)
:Evidence: ompi/mpi/tool/source_get_num.c:36 (\*num_source=0 unconditionally - the only num_source assignment in the tree); source_get_info.c and source_get_timestamp.c return MPI_T_ERR_INVALID_INDEX for any index; no source-registration framework anywhere in opal/mca/base, ompi/mca, configure, or Makefiles
:Notes: Conflict resolved to PARTIAL. The source functions compile unconditionally into libmpi (default build, no flag) and are individually conformant for an empty space: the standard mandates no minimum source count, so num_source=0 and INVALID_INDEX for any index are correct vacuous results (b2/b3 correct on conformance-of-the-empty-space). BUT the source feature is functionally never provided - zero sources are ever registered and there is no flag/component/library path to register any - so source ordering, ticks_per_second, and max_ticks machinery is never exercised. Symbols present + permanently empty + no enable path = PARTIAL (b1 correct that it is functionally absent, though UNSUPPORTED overstates it since the empty-space behavior is standard-permitted). Aligned with the events sub-interface verdict.

MPI_T event types: enumerate (event_get_num/get_info/get_index) and event-registration handle alloc/free
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §15.3.8 / p.748
:Reviewers: b1=UNSUPPORTED, b2=UNSUPPORTED, b3=SUPPORTED (reviewers disagreed)
:Evidence: ompi/mpi/tool/event_get_num.c:36 (\*num_event=0 unconditionally); event_get_info.c and event_handle_alloc.c return MPI_T_ERR_INVALID_INDEX; event_get_index.c returns MPI_T_ERR_INVALID_NAME; event_handle_free.c returns MPI_T_ERR_INVALID_HANDLE; no mca_base_event infrastructure exists (mca_base_event appears ONLY in mpi.h.in/mpi.h comments, never as code)
:Notes: Conflict resolved to PARTIAL. All event-enumeration symbols are present in the default build and compile unconditionally (Makefile.am interface_profile_sources). Returning an empty event space (num_event=0) is standard-permitted, so the enumeration path is conformant-but-empty (b3's point). But there is no mca_base_event framework, no MCA component registers any event, and no configure flag/library enables event delivery - the marquee MPI-4.0/5.0 facility is permanently inert. b2 correctly noted there is NO conditional path (status missing_gap, i.e. no flag enables it); the framework taxonomy maps 'present but incomplete/not fully standard-conformant' to PARTIAL rather than UNSUPPORTED because the symbols exist and the empty-space behavior is conformant. Not MISSING_INFLIGHT: PR #13280 is the ABI chapter and adds zero event infrastructure.

MPI_T event callbacks: register_callback, callback_get_info/set_info, handle_get_info/set_info, callback-safety-level (MPI_T_CB_REQUIRE\_\*) dispatch
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §15.3.8 / p.752-754
:Reviewers: b1=UNSUPPORTED, b2=UNSUPPORTED, b3=SUPPORTED (reviewers disagreed)
:Evidence: ompi/mpi/tool/event_register_callback.c:35 (unconditional MPI_T_ERR_INVALID_HANDLE); event_callback_get_info.c / set_info.c / event_handle_get_info.c / set_info.c all return MPI_T_ERR_INVALID_HANDLE; MPI_T_CB_REQUIRE\_{NONE,MPI_RESTRICTED,THREAD_SAFE,ASYNC_SIGNAL_SAFE} constants present in mpi.h.in:949-952 (map to OPAL_MCA_BASE_CB_REQUIRE\_\*)
:Notes: Conflict resolved to PARTIAL. The callback-safety constants are defined and distinct in a default build, but since no event type can be enumerated and no registration handle can be allocated, no callback is ever registered or invoked and the safety-level dispatch path (Table 15.5) is dead code. Symbols present, behavior inert, no enable path -> PARTIAL, aligned with the events sub-interface verdict.

MPI_T dropped-event handling: MPI_T_event_set_dropped_handler and dropped-callback ordering
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §15.3.8 / p.755
:Reviewers: b1=UNSUPPORTED, b2=silent, b3=SUPPORTED (reviewers disagreed)
:Evidence: ``ompi/mpi/tool/event_set_dropped_handler.c (unconditional MPI_T_ERR_INVALID_HANDLE after init check)``
:Notes: Conflict resolved to PARTIAL. Symbol present in default build but inert: events are never raised so dropped-event semantics are never implemented or exercised. No enable path. Aligned with the events sub-interface verdict.

Reading event data/metadata inside callbacks: MPI_T_event_read / event_copy / event_get_timestamp / event_get_source
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Partial**
:Standard: §15.3.8 / p.756-759
:Reviewers: b1=UNSUPPORTED, b2=silent, b3=SUPPORTED (reviewers disagreed)
:Evidence: ``ompi/mpi/tool/event_read.c, event_copy.c, event_get_timestamp.c, event_get_source.c all return MPI_T_ERR_INVALID_HANDLE unconditionally after the init check``
:Notes: Conflict resolved to PARTIAL. Symbols present in default build but inert: callbacks never fire, so these accessors are never reachable with a valid event instance. Empty-space behavior is conformant but the data-reading machinery is non-functional with no enable path. Aligned with the events sub-interface verdict.

MPI_T defined with C bindings only (no Fortran/f08 bindings)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **N/A**
:Standard: §15.3 / general
:Reviewers: b1=silent, b2=SUPPORTED, b3=silent (raised by 1 reviewer)
:Evidence: for-claude/mpi-standard-5.0-apis.json:46164-46180 (MPI_T_category_get_events f08_expressible=false); standard ch15 defines no Fortran 2008 binding for any MPI_T routine; all MPI_T procs c_present=true
:Notes: Unique to b2; verified. The standard deliberately defines no Fortran/f08 bindings for MPI_T routines, so universal f08 absence is correct by-design, not a coverage gap. Classified NA (a requirement that does not bind the implementation to provide Fortran bindings). Only MPI_Pcontrol, a 15.2 routine, has Fortran bindings, which are present.

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - PMPI name-shifted entry points (PMPI\_ prefix) for every MPI/MPI_T procedure in every provided binding
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Profiling wrappers separable from the base library (a profiling library can be linked without name clashes)
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Pcontrol provided as a conformant no-op in the MPI library
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Pcontrol init/finalize availability guard
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - MPI_T_init_thread / MPI_T_finalize: reference-counted, callable before MPI_Init and after MPI_Finalize, returns provided thread level
     - b1=SUPP/b2=SUPP/b3=PART (conflict)
   * - MPI_T enumeration types (MPI_T_enum) query: enum_get_info / enum_get_item
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_T required datatype subset (Table 15.3) usable before MPI_Init
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Control variables: enumerate / get_info / get_index / handle alloc-free / read / write honoring scope
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Performance variables: classes / get_info / sessions / handle alloc-free / start / stop / read / write / reset
     - b1=PART/b2=SUPP/b3=sile (conflict)
   * - Variable categorization: category enumerate / get_info / get_cvars / get_pvars / get_categories / category_changed
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_T tool-info routines return unique MPI_T_ERR\_\* codes without invoking error handlers (non-fatal)
     - b1=PART/b2=sile/b3=sile (unique)
