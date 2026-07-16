<!--
  Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.

  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# OMPIO MPI_Info Support Tasks

This implementation is intentionally split into five waves. Each wave
should leave the tree buildable, runnable, and reviewable before the
next wave starts.

When asked to "implement phase N", complete all tasks in that phase,
run the listed verification for that phase, and stop before starting
the next phase.

## Phase 1: Core Infrastructure

Goal: establish the OMPIO info plumbing with one simple OMPIO-owned
hint so the model can be reviewed independently.

Implementation tasks:

- [x] Audit current OMPIO-related info calls and record the initial
  hint inventory in the implementation notes or commit message:
  - `ompi/mca/common/ompio/`
  - `ompi/mca/io/ompio/`
  - `ompi/mca/fs/`
  - `ompi/mca/fcoll/`
  - `ompi/mca/fbtl/`
  - `ompi/mca/sharedfp/`
- [x] Add OMPIO-local helper declarations and implementation under the
  appropriate `ompi/mca/common/ompio/` files.
- [x] Add helper support for:
  - subscribing a hint on the backing `ompi_file_t`,
  - applying a user info object through `opal_infosubscribe_change_info()`,
  - setting public implementation hints,
  - duplicating public current hints for `MPI_File_get_info`.
- [x] Add an OMPIO info phase enum for open, set-info, and set-view.
- [x] Add the current info phase to `ompio_file_t`.
- [x] Ensure `ompio_file_t::f_info` is a borrowed pointer to
  `ompi_file_t::super.s_info`.
- [x] Add `mca_io_ompio_file_set_info()`.
- [x] Add `mca_io_ompio_file_get_info()`.
- [x] Wire both functions into `mca_io_ompio_module`.
- [x] Register exactly one simple OMPIO-owned hint, preferably
  `cb_buffer_size`, including its default.
- [x] Reapply open-time user info after the Phase 1 subscription is
  installed.
- [x] Make `MPI_File_get_info` return public current hints from
  `fh->super.s_info`.

Behavior at the end of Phase 1:

- OMPIO has its own get-info and set-info hooks.
- `MPI_File_get_info` on an OMPIO file can return at least the one
  registered defaulted hint.
- Unknown user hints are not returned.
- Existing OMPIO behavior for other hints is not intentionally changed.

Verification:

- [x] Build the touched OMPIO/common OMPIO sources.
- [x] Verify the touched OMPIO files are compiled in the local
  configuration.
- [x] Manually run a tiny MPI-IO smoke test with `--mca io ompio`.
- [x] Verify `MPI_File_get_info` returns the Phase 1 defaulted hint
  after opening with `MPI_INFO_NULL`.
- [x] Verify an unknown open-time hint is not returned.
- [x] Review memory ownership:
  - `fh->super.s_info` remains owned by `ompi_file_t`,
  - `ompio_file_t::f_info` is borrowed,
  - returned `MPI_Info` objects are caller-owned.

## Phase 2: OMPIO Core Hints and Manual Test

Goal: convert OMPIO common/core hints to the new mechanism and add a
manual `test/simple` regression test for generic OMPIO behavior.

Implementation tasks:

- [x] Register `cb_buffer_size` if not completed in Phase 1.
- [x] Register `cb_nodes`.
- [x] Register `collective_buffering`.
- [x] Move OMPIO common/core hint parsing to callbacks or helper-backed
  accessors.
- [x] Preserve existing MCA-parameter override behavior.
- [x] Preserve current runtime behavior for:
  - aggregator selection,
  - collective buffering,
  - collective I/O buffer sizing.
- [x] Ensure omitted hints survive `MPI_File_set_info`.
- [x] Ensure ignored/unknown hints do not become public.
- [x] Add a standalone test under `test/simple`.
- [x] Make the test build consistently with existing `test/simple`
  conventions.
- [x] Cover these generic OMPIO cases in the test:
  - open with `MPI_INFO_NULL` and verify defaulted supported hints,
  - open with `cb_buffer_size` and verify the returned value,
  - open with an unknown key and verify it is not returned,
  - call `MPI_File_set_info` with a supported mutable hint and verify
    the update,
  - call `MPI_File_set_info` with an unknown key and verify previous or
    default hints remain unchanged.
- [x] Document manual invocation in the test source comment or nearby
  test notes, for example:

  ```sh
  mpirun -n 2 --mca io ompio ./ompio_file_info
  ```

Behavior at the end of Phase 2:

- OMPIO common/core hints are handled through the existing
  `opal_infosubscriber_t` mechanism.
- Generic `MPI_File_get_info` / `MPI_File_set_info` behavior is
  covered by a manually runnable test.
- The tree remains useful even without Lustre or GPFS.

Verification:

- [x] Build OMPIO and the new `test/simple` test.
- [x] Run the new test with `mpirun -n 2 --mca io ompio`.
- [x] Run the test with supported and unsupported hints.
- [x] Verify no generated MPI binding files were hand-edited.
- [x] Check for warnings in touched code.

## Phase 3: MPI_File_set_view Integration

Goal: route `MPI_File_set_view` info handling through the same
subscription path while keeping the tree fully runnable.

Implementation tasks:

- [x] Update `mca_io_ompio_file_set_view()` to apply its `info`
  argument through the OMPIO helper layer.
- [x] Set phase to set-view while applying view info.
- [x] Preserve existing hints not mentioned in the view info object.
- [x] Remove ad hoc copying of accepted view hints into `fh->f_info`.
- [x] Verify view setup reads accepted/current hints from
  `ompio_fh->f_info`.
- [x] Extend the `test/simple` test to cover set-view accepted hint
  handling.
- [x] Confirm `cb_nodes` and `collective_buffering` semantics are
  unchanged for view setup.

Behavior at the end of Phase 3:

- Open, set-info, and set-view all use the same OMPIO info mechanism
  for OMPIO common/core hints.
- Existing view setup behavior is preserved.
- The manual test covers set-view info behavior.

Verification:

- [x] Build touched OMPIO files.
- [x] Run the manual `test/simple` test with `--mca io ompio`.
- [x] Run at least one smoke MPI-IO program that sets a non-default
  file view.
- [x] Confirm omitted hints survive set-view.
- [x] Confirm unknown set-view hints are not returned by get-info.

## Phase 4: Component-Owned Hints

Goal: extend the mechanism to selected subcomponents without hard-coding
their hint names in the OMPIO framework.

Implementation tasks:

- [x] Add `sharedfp/individual` subscription for
  `OMPIO_SHAREDFP_RELAXED_ORDERING`.
- [x] Move existing direct `fh->f_info` parsing in
  `sharedfp/individual` to callback/helper logic.
- [x] Preserve current sharedfp component priority/selection behavior.
- [~] Add `fs/lustre` subscriptions for:
  - `striping_unit`
  - `striping_factor`
  - `stripe_size`
  - `stripe_width`
- [~] Preserve accepted Lustre alias spelling in returned info.
- [~] Treat Lustre layout hints as open/create-time hints unless the
  component can safely apply them later.
- [~] Add `fs/gpfs` subscriptions for supported GPFS/SIOX hints when
  GPFS support is built.
- [x] Keep filesystem-specific hint names inside their owning
  components.
- [~] Define how each alias stores its canonical relationship.
- [~] Use canonical public spelling only when returning defaults with no
  user-supplied spelling.
- [x] Extend the manual test for alias spelling and component-owned
  hints where the relevant component is available.
- [x] Make filesystem-specific test sections skip gracefully when the
  relevant component/filesystem is unavailable.

Behavior at the end of Phase 4:

- OMPIO common/core, `sharedfp/individual`, `fs/lustre`, and `fs/gpfs`
  use component-owned hint registration.
- OMPIO framework/common code does not hard-code hint names owned by
  those components.
- Filesystem-dependent behavior remains conditional and reviewable.

Verification:

- [x] Build touched OMPIO and sharedfp files.  The edited Lustre/GPFS
  component sources could not be built in this local configuration
  because the Lustre and GPFS development headers are unavailable.
- [x] Run the manual `test/simple` test on a generic filesystem.
- [ ] Run Lustre-specific checks only on Lustre or with a build where
  the component can be selected safely.
- [ ] Run GPFS-specific checks only when GPFS support is built and
  available.
- [x] Verify ignored component-specific hints are not returned.
- [x] Verify accepted aliases preserve user spelling where locally
  testable; Lustre alias runtime verification requires a Lustre-enabled
  build and filesystem.
- [x] Review for memory ownership and temporary `opal_cstring_t`
  releases.

## Phase 5: Documentation, Release Note, and Final Cleanup

Goal: document the user-visible behavior and perform final consistency
checks once the implementation behavior is settled.

Implementation tasks:

- [x] Add an `MPI_Info hints` section to
  `docs/tuning-apps/mpi-io.rst`.
- [x] Document hints in tables grouped by owner/component.
- [x] Include accepted phases, defaults, MCA parameter mappings, and
  notes.
- [x] Document alias-preservation behavior.
- [x] Add cross-references from:
  - `docs/man-openmpi/man3/MPI_File_open.3.rst`
  - `docs/man-openmpi/man3/MPI_File_set_view.3.rst`
  - `docs/man-openmpi/man3/MPI_File_set_info.3.rst`
  - `docs/man-openmpi/man3/MPI_File_get_info.3.rst`
- [x] Add a shared include file so the same hint documentation appears
  in the tuning guide and in the rendered MPI-IO man pages.
- [x] Add a release-note/changelog entry under
  `docs/release-notes/changelog/` if the behavior is deemed
  user-visible.
- [x] Add rationale comments to the non-obvious implementation paths,
  especially where callbacks, cached info values, and staged updates
  preserve MPI semantics.
- [x] Review all touched code for Open MPI style:
  - config header first,
  - constants on the left in equality checks,
  - braced blocks,
  - 4-space indentation,
  - no new global symbols without appropriate prefixes.
- [x] Confirm no component-specific hint names are hard-coded in OMPIO
  framework/common code unless OMPIO common/core owns them.
- [x] Confirm generated MPI binding files were not hand-edited.
- [x] Confirm no new memory ownership ambiguity:
  - `fh->super.s_info` remains owned by `ompi_file_t`,
  - `ompio_file_t::f_info` is borrowed,
  - returned `MPI_Info` objects are caller-owned,
  - temporary `opal_cstring_t` values are released.

Behavior at the end of Phase 5:

- The implementation is documented for users.
- Manual test coverage exists for the implemented behavior.
- The work is ready for full review.

Verification:

- [~] Build the touched code that is included in this local
  configuration.  The edited Lustre/GPFS component sources could not be
  built locally because the corresponding development headers are not
  installed, matching the Phase 4 verification caveat.
- [x] Run the manual `test/simple` test with `--mca io ompio`.
- [x] Run documentation build checks if Sphinx is enabled.
- [x] If build-system files were not changed, avoid unnecessary
  `autogen.pl`; if they were changed, regenerate according to
  `AGENTS.md`.
- [x] Prepare a concise final test/verification summary for reviewers.
