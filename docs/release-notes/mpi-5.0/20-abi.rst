.. _release-notes-mpi-5.0-ch20:

Application Binary Interface (ABI)
==================================


.. admonition:: tl;dr
   :class: tip

   MPI-5.0 Chapter 20 defines an opt-in standard ABI: fixed integer/Huffman-coded handle and constant values, an 8-int (32-byte) C MPI_Status, fixed integer typedefs, version/info query functions, Fortran type/boolean registration functions, MPI\_<Type>_toint/_fromint serializers for all 11 handle types (22 functions), the MPI_ABI_VERSION/MPI_ABI_SUBVERSION/MPI_ERR_ABI constants, and dedicated libmpi_abi / mpifort_abi libraries plus an mpicc_abi wrapper. The ENTIRE chapter is absent on main and is supplied by in-flight PR #13280 (https://github.com/open-mpi/ompi/pull/13280): verified zero hits for MPI_Abi_get/set, the 22 \*_toint/\*_fromint converters, and MPI_ABI_VERSION/SUBVERSION/ERR_ABI across ompi/, and no abi/toint/fromint binding files in ompi/mpi/c/. Every requirement is therefore MISSING_INFLIGHT (never UNSUPPORTED — in-flight, not abandoned). Open MPI's native ABI legitimately differs from the standard ABI (pointer-valued handles to ompi\_\*_t tags, a 24-byte LP64 status with size_t _ucount, MPI_STATUS_SIZE=6, error classes ending at MPI_ERR_ERRHANDLER=80/LASTCODE=92); the standard explicitly permits this parallel-ABI coexistence (20.1, 20.2), so those differences are context, not gaps. Draft generator stubs at ompi/mpi/bindings/ompi_bindings/c.py:262-264 (MPI_Abi_details/supported/version) use pre-standard names, are unwired, and satisfy no chapter requirement.

Conformance summary
-------------------


**18** reconciled requirement(s)/behavior(s): 0 supported, 0 conditional, 0 partial, 0 unsupported, 18 missing-but-in-flight, 0 N/A (0.0% of applicable fully supported).

Caveats, gaps, and reviewer conflicts
-------------------------------------


Standard-ABI header (mpi.h) declaring the full ABI surface; excludes pre-MPI-3.1 deprecated features
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.2.1 / p.898
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ``ompi/include/mpi.h.in (single native header; no separate standard-ABI header; no MPI_ABI_* tags / *_toint / *_fromint / MPI_ABI_VERSION declared)``
:Notes: Token conflict (b1 UNSUPPORTED vs b2 MISSING_INFLIGHT); b1 itself carried in_flight=#13280 on every item but its rubric lacks a missing_inflight bucket. Resolved by the in-flight tiebreaker (canonical Ch20 case): the standard-ABI header is added by PR #13280, so MISSING_INFLIGHT, not UNSUPPORTED. Native default mpi.h.in is the implementation-specific ABI, which 20.1/20.2 permit to coexist.

MPI_Abi_get_version + MPI_ABI_VERSION / MPI_ABI_SUBVERSION macros (returns ABI major/minor, -1 if unsupported; thread-safe; callable any time)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.2 / p.896
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=MISSING_INFLIGHT (reviewers disagreed)
:Evidence: ``grep -rIiE 'MPI_Abi_(get|set)' ompi/ → 0 hits; grep -nE 'MPI_ABI_VERSION|MPI_ABI_SUBVERSION' ompi/include/mpi.h.in → 0 hits; no abi binding file in ompi/mpi/c/``
:Notes: b2/b3 MISSING_INFLIGHT vs b1 UNSUPPORTED (token only). Resolved by in-flight tiebreaker → MISSING_INFLIGHT. The draft generator stub MPI_Abi_version at ompi/mpi/bindings/ompi_bindings/c.py:264 uses pre-standard naming (no 'get'), is unwired, and does NOT make the conformant symbol present — the standardized function/macros are added by PR #13280.

MPI_Abi_get_info (info object with keys mpi_aint_size / mpi_count_size / mpi_offset_size; thread-safe; callable any time)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.2 / p.897
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=MISSING_INFLIGHT (reviewers disagreed)
:Evidence: ``no MPI_Abi_get_info in ompi/include/mpi.h.in or ompi/mpi/c/ (grep 0 hits); no binding file``
:Notes: Token conflict resolved by in-flight tiebreaker → MISSING_INFLIGHT. Predefined info-key values are asserted by PR #13280, not independently verifiable on this checkout; the symbol is verifiably absent.

Standard-ABI library naming (libmpi_abi as sole direct dependency of the app binary)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.2.1 / p.898
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ``ompi/Makefile.am builds libmpi only; no libmpi_abi target on main``
:Notes: Token conflict resolved by in-flight tiebreaker → MISSING_INFLIGHT. PR #13280 adds libmpi_abi (ompi/mpi/c/Makefile_abi.include).

mpicc_abi (standard-ABI compiler wrapper)
"""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.2.1 / p.898 (advice to implementors)
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ``ompi/tools/wrappers/ has only mpicc-wrapper-data.txt[.in]; no mpicc_abi wrapper data``
:Notes: Framed as advice-to-implementors but concretely part of PR #13280 (mpicc_abi-wrapper-data.txt.in). Kept MISSING_INFLIGHT (not NA) to keep the in-flight sweep consistent, since the artifact ships in the PR.

C MPI_Status standard-ABI layout (struct of exactly 8 ints: MPI_SOURCE, MPI_TAG, MPI_ERROR, int MPI_internal[5]; 32 bytes LP64)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.3.1 / p.898
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ompi/include/mpi.h.in:475-487 struct ompi_status_public_t = {int MPI_SOURCE,MPI_TAG,MPI_ERROR; int _cancelled; size_t _ucount} = 24 bytes LP64, NOT the 8-int/32-byte standard layout
:Notes: Token conflict resolved by in-flight tiebreaker → MISSING_INFLIGHT. Native 24-byte status is a permitted parallel-ABI realization (20.1/20.2), not a violation; the 8-int standard-ABI status is supplied by PR #13280. Exact PR layout asserted, not verified on this checkout.

Opaque handles as incomplete-struct pointers with MPI_ABI\_\* tag names (all 11 handle types: Comm, Datatype, Group, Request, File, Win, Op, Info, Errhandler, Message, Session)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.3.2 / p.899
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ompi/include/mpi.h.in:452-468 native tags (typedef struct ompi_communicator_t \*MPI_Comm, ompi_datatype_t \*MPI_Datatype, ...); no MPI_ABI_Comm/MPI_ABI_Datatype tags present (grep 0 hits)
:Notes: Native handles already use the incomplete-struct-pointer model (structurally ABI-compatible) but with native struct tags. The standard-ABI MPI_ABI\_\* tag names are absent and added by PR #13280 → MISSING_INFLIGHT (in-flight tiebreaker over b1's UNSUPPORTED).

Predefined handle constants as compile-time integer expressions cast to handle type (Huffman-coded values 1..4095; reserved range never used for user handles; integer 0 never valid)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.3.3, §20.5 / pp.899,907-912
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ``ompi/include/mpi.h.in:1282-1283 MPI_COMM_WORLD = OMPI_PREDEFINED_GLOBAL(...) (address of ompi_mpi_comm_world global), not a 1-4095 integer cast``
:Notes: Native predefined handles are link-time global-object pointers, not the standard ABI's compile-time Huffman integers — a permitted parallel-ABI difference. The standard fixed values are supplied by PR #13280 → MISSING_INFLIGHT. Full constant-value tables asserted, not independently verifiable here.

Standard-ABI integer constant scheme + MPI_ERR_ABI error class (group-unique values, unused values reserved to 16384; MPI_ANY_SOURCE etc. negative; MPI_ERR_ABI defined)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.3.4, Annex A / p.900
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=MISSING_INFLIGHT (reviewers disagreed)
:Evidence: ``ompi/include/mpi.h.in:767,772 native error classes end at MPI_ERR_ERRHANDLER=80, MPI_ERR_LASTCODE=92; grep -nE 'MPI_ERR_ABI' ompi/include/mpi.h.in → 0 hits``
:Notes: MPI_ERR_ABI absent; native error-class numbering is implementation-specific and need not follow the standard-ABI scheme (permitted parallel-ABI difference). Token conflict (b1 UNSUPPORTED) resolved by in-flight tiebreaker → MISSING_INFLIGHT; PR #13280 adds MPI_ERR_ABI and the reserved scheme.

Standard-ABI integer types (MPI_Aint=intptr_t, MPI_Offset=int64_t, MPI_Count=int64_t)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.3.5 / p.900
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ompi/include/mpi.h native typedefs (ptrdiff_t / long long) are size-equivalent on LP64 but are the native-ABI forms, not the standard-ABI fixed intptr_t/int64_t typedefs
:Notes: Native typedefs are configuration-dependent and size-equivalent but not the standard-ABI realization; the fixed forms are part of the standard-ABI header added by PR #13280 → MISSING_INFLIGHT (in-flight tiebreaker over b1's UNSUPPORTED).

Predefined datatype / op / handle constant values follow Huffman category-instance tables (Tables 20.1-20.11; fixed-size types encode log2(size) in bits 5:3)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.5 / pp.909-912
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ompi/include/mpi.h.in native datatypes/ops are pointers to ompi_predefined_datatype_t / ompi_predefined_op_t globals (OMPI_PREDEFINED_GLOBAL), no Huffman encoding
:Notes: Merges b1's two Huffman-table items (datatypes vs ops/handles). Native predefined handles are global-object pointers, not Huffman integers — permitted parallel-ABI difference. Tables supplied by PR #13280 → MISSING_INFLIGHT. Table values asserted, not independently verifiable here.

Optional predefined datatype runtime detection (every predefined datatype has an ABI value; absent optional types detected when MPI_Type_size returns MPI_UNDEFINED)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.4 / p.901
:Reviewers: b1=silent, b2=MISSING_INFLIGHT, b3=silent (raised by 1 reviewer)
:Evidence: ``standard-ABI header (with all predefined datatype constant values present) is absent on main``
:Notes: Unique to b2; valid. This runtime-detection behavior is meaningful only under the standard ABI where every predefined datatype constant is defined; that header is supplied by PR #13280 → MISSING_INFLIGHT.

MPI_Abi_set_fortran_info / MPI_Abi_get_fortran_info (convey Fortran type sizes & \*_supported flags via predefined info keys; first SET wins, later SET calls return MPI_ERR_ABI)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.4.1 / pp.901-905
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=MISSING_INFLIGHT (reviewers disagreed)
:Evidence: ``grep -rIiE 'MPI_Abi_(get|set)' ompi/ → 0 hits; no abi_*_fortran_info binding file in ompi/mpi/c/``
:Notes: Both functions absent on main. Token conflict (b1 UNSUPPORTED) resolved by in-flight tiebreaker → MISSING_INFLIGHT. PR #13280 adds them; b3 notes the Fortran ABI registration path is incomplete even in that PR. Key set and first-call-wins semantics asserted, not verified here.

MPI_Abi_set_fortran_booleans / MPI_Abi_get_fortran_booleans (convey LOGICAL size and .TRUE./.FALSE. literals with is_set flag; first SET wins, later return MPI_ERR_ABI)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.4.1 / pp.903-905
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=MISSING_INFLIGHT (reviewers disagreed)
:Evidence: ``grep -rIiE 'MPI_Abi_(get|set)' ompi/ → 0 hits; no abi_*_fortran_booleans binding file in ompi/mpi/c/``
:Notes: Both functions absent on main. Token conflict resolved by in-flight tiebreaker → MISSING_INFLIGHT. PR #13280 adds them; Fortran ABI path incomplete in that PR. Semantics asserted, not verified here.

Standard-ABI Fortran modules (mpi, mpi_f08) and library mpifort_abi following 20.2.1 naming/versioning/dependency rules
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.4.2 / p.905
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=MISSING_INFLIGHT (reviewers disagreed)
:Evidence: ``ompi/mpi/fortran/ builds native libmpi_mpifh and use-mpi-f08 only; no mpifort_abi target on main``
:Notes: Token conflict resolved by in-flight tiebreaker → MISSING_INFLIGHT. b2/b3 caveat: PR #13280's enumerated artifacts emphasize the C ABI (libmpi_abi, mpicc_abi) and the Fortran ABI (mpifort_abi + full type registration) is incomplete even in the PR; still classified MISSING_INFLIGHT since the chapter target is in-flight, not abandoned.

Handle serialization MPI\_<Type>_toint / MPI\_<Type>_fromint for all 11 handle types (22 functions): predefined handles map to fixed Annex A values, user handles map deterministically outside reserved range, do not depend on MPI_Fint; erroneous on invalid/freed handles
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.4.5 / pp.907-908
:Reviewers: b1=UNSUPPORTED, b2=MISSING_INFLIGHT, b3=MISSING_INFLIGHT (reviewers disagreed)
:Evidence: grep -rIE 'MPI\_(Comm\|Type\|Group\|Request\|File\|Win\|Op\|Info\|Errhandler\|Message\|Session)_(toint\|fromint)' ompi/ → 0 hits; no \*_toint/\*_fromint binding files in ompi/mpi/c/
:Notes: Collapses all 22 converters (b3's 22 flagged symbols + b1's item) into one topic. 11 handle types / 22 functions (Comm, Datatype, Group, Request, File, Win, Op, Info, Errhandler, Message, Session); b2's '12 classes / 24 functions' over-counts and was not propagated. All absent on main; C-only by design (depends only on C features, not MPI_Fint). Token conflict resolved by in-flight tiebreaker → MISSING_INFLIGHT; added by PR #13280.

Standard-ABI Fortran status layout (bind(C) type of 8 INTEGERs; MPI_STATUS_SIZE=8, MPI_F_STATUS_SIZE=8, MPI_F_SOURCE/TAG/ERROR=0/1/2)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.4.3 / p.906
:Reviewers: b1=PARTIAL, b2=MISSING_INFLIGHT, b3=silent (reviewers disagreed)
:Evidence: ompi/include/mpif-config.h:53 MPI_STATUS_SIZE=6 (NOT 8); ompi/include/mpi.h.in:603-605 MPI_F_SOURCE/TAG/ERROR=0/1/2 (coincidentally match); no 8-INTEGER bind(C) standard-ABI status type present
:Notes: b1 marked PARTIAL because the index constants (0/1/2) match while the size (6 vs 8) and 8-INTEGER bind(C) layout do not. Per the in-flight tiebreaker this does NOT become PARTIAL: the matching MPI_F_SOURCE/TAG/ERROR=0/1/2 is coincidental native behavior, not partial standard-ABI support, and the conforming 8-INTEGER layout + MPI_STATUS_SIZE=8 are supplied entirely by PR #13280 → MISSING_INFLIGHT.

ABI rule: MPI_F_STATUS_IGNORE / MPI_F08_STATUS_IGNORE sentinels NOT specified in the standard-ABI C header
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

:Status: **Missing (in-flight)**
:Standard: §20.4.4 / p.906
:Reviewers: b1=UNSUPPORTED, b2=silent, b3=silent (raised by 1 reviewer)
:Evidence: ``ompi/include/mpi.h.in:1246,1249 native MPI_F_STATUS_IGNORE / MPI_F08_STATUS_IGNORE ARE declared in the C header (opposite of the ABI rule)``
:Notes: Unique to b1; valid. Native behavior declares these sentinels in the C header — the opposite of the 20.4.4 ABI rule — but that is permitted native ABI, not a violation. The standard-ABI C header that omits them is part of PR #13280 → MISSING_INFLIGHT. (b1's original mpif-sentinels.h:54-56 citation was inaccurate per b1's own corrected caveat; that file holds the unrelated MPI_STATUS_IGNORE array sentinel.)
