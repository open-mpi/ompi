<!--
  Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.

  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# OMPIO MPI_Info Hint Plan

## Goal

Make OMPIO comply with the MPI-5.0 `MPI_File_get_info` and
`MPI_File_set_info` requirements by using Open MPI's existing
`opal_infosubscriber_t` infrastructure.

This work addresses Open MPI issue #13367, "`MPI_File_get_info`
fails to return hints as required by MPI standard":
https://github.com/open-mpi/ompi/issues/13367

The implementation should:

- Return all supported MPI-IO hints with default values.
- Return user-supplied hints that OMPIO and its selected subcomponents
  accepted.
- Return additional hints set by the implementation.
- Avoid returning ignored or unknown user hints.
- Preserve user-supplied alias spelling when an alias is accepted.
- Support hints at `MPI_File_open`, `MPI_File_set_info`, and
  `MPI_File_set_view` time as appropriate for each hint.
- Avoid a parallel OMPIO-private hint cache.

## MPI-5.0 Requirements

The governing text is the MPI-5.0 standard, section 15.2.8 ("File
Info"), which defines `MPI_FILE_SET_INFO` and `MPI_FILE_GET_INFO`;
section 15.2.8.1 ("Reserved File Hints") lists the reserved hint keys.

Per section 15.2.8, `MPI_File_get_info` returns a newly created info
object containing the current setting of all hints associated with the
file. The standard requires the returned object to include:

- Hints supported by the implementation that have specified default
  values.
- User-supplied hints that were not ignored.
- Additional hints set by the implementation.

The returned info object is caller-owned and must be freed by the
caller with `MPI_Info_free`.

Section 15.2.8 also requires `MPI_File_set_info` to update file hints
without disturbing previous/default values for hints not present in
the new info object. If a supplied hint is ignored, the previous value
or default remains in effect.

## Existing Infrastructure

Use the existing `opal_infosubscriber_t` mechanism:

- `opal_infosubscribe_subscribe()` registers supported keys, default
  values, and callbacks.
- `opal_infosubscribe_change_info()` applies a user info object through
  registered callbacks.
- `opal_info_dup_public()` produces a public copy suitable for
  `MPI_File_get_info`.
- `opal_info_set()` / `opal_info_set_cstring()` store accepted public
  hints.

`ompi_file_t::super.s_info` should be the authoritative current hint
object. `ompio_file_t::f_info` should remain a borrowed pointer to that
same object.

Do not create a second OMPIO-owned list of hints.

## OMPIO Helper Layer

Add thin OMPIO-local helpers, likely under `ompi/mca/common/ompio/`,
to keep call sites clean:

- Subscribe a hint on the backing `ompi_file_t`.
- Apply a user info object for a specific phase.
- Set public implementation hints.
- Duplicate public current hints for `MPI_File_get_info`.

These helpers should wrap the existing `opal_infosubscriber_t` and
`opal_info_t` APIs. They should not implement a new shared info
framework.

## Hint Context

Add a small OMPIO hint phase/context to `ompio_file_t`, for example:

- Open.
- Set-info.
- Set-view.

Callbacks can use the phase to decide whether a hint is valid in the
current operation. For example, filesystem layout hints may be accepted
during file creation/open but ignored after the file already exists.

## Component Ownership

Each selected component should register its own hints and defaults.
The OMPIO core should not hard-code hint names owned by `fs`, `fcoll`,
`fbtl`, or `sharedfp` components.

Initial hint owners to convert:

- OMPIO common/core:
  - `cb_buffer_size`
  - `cb_nodes`
  - `collective_buffering`

- `fs/lustre`:
  - `striping_unit`
  - `striping_factor`
  - `stripe_size` alias
  - `stripe_width` alias

- `sharedfp/individual`:
  - `OMPIO_SHAREDFP_RELAXED_ORDERING`

- `fs/gpfs`, when built:
  - GPFS and SIOX keys currently consumed in
    `ompi/mca/fs/gpfs/fs_gpfs_file_set_info.c`

Other components can be added by registering their own hints through
the same mechanism.

## File Open Flow

During file open:

1. `ompi_file_open()` creates `fh->super.s_info`.
2. OMPIO sets `ompio_fh->f_info = fh->super.s_info`.
3. OMPIO selects the relevant subcomponents.
4. Selected components subscribe their supported hints and defaults.
5. OMPIO applies the original user info through
   `opal_infosubscribe_change_info()`.
6. OMPIO and subcomponents consume accepted/current hints from
   `fh->super.s_info` / `ompio_fh->f_info`.

Defaults must be registered before user hints are applied so that user
hints can override defaults, and so that `MPI_File_get_info` can return
supported defaulted hints even when the user passed `MPI_INFO_NULL`.

## MPI_File_set_info Flow

Add OMPIO module hooks:

- `mca_io_ompio_file_set_info`
- `mca_io_ompio_file_get_info`

`mca_io_ompio_file_set_info` should:

1. Lock the file handle.
2. Set the OMPIO info phase to set-info.
3. Apply the user info through `opal_infosubscribe_change_info()`.
4. Let subscribed component callbacks accept, ignore, or transform
   values.
5. Let callbacks update component state for mutable hints.
6. Leave omitted hints unchanged.
7. Leave ignored hints at their previous/default values.
8. Unlock and return.

Hints that cannot be changed after open should be ignored in this
phase by their owning component.

## MPI_File_get_info Flow

`mca_io_ompio_file_get_info` should:

1. Allocate a new `ompi_info_t`.
2. Duplicate public current hints from `fh->super.s_info` with
   `opal_info_dup_public()`.
3. Return the new object through `info_used`.

OMPIO must not retain ownership of the returned object.

## MPI_File_set_view Flow

Route the `info` argument to `MPI_File_set_view` through the same
subscription mechanism:

1. Set the OMPIO info phase to set-view.
2. Apply the view info through `opal_infosubscribe_change_info()`.
3. Run view setup using accepted/current hints from `fh->super.s_info`.
4. Preserve existing hints not mentioned by the set-view info object.

This should replace ad hoc patterns where `mca_common_ompio_set_view`
reads raw input info and manually copies accepted keys into
`fh->f_info`.

## Alias Handling

If a user supplies an accepted alias, preserve the supplied spelling in
`MPI_File_get_info`.

For default values with no user-supplied spelling, use the canonical
public key for that hint.

## Documentation

Update `docs/tuning-apps/mpi-io.rst` with a dedicated `MPI_Info hints`
section.

Document hints in tables grouped by owner/component. Each row should
include:

- Hint name.
- Owner/component.
- Whether it is accepted at `MPI_File_open`.
- Whether it is accepted at `MPI_File_set_info`.
- Whether it is accepted at `MPI_File_set_view`.
- Default value, if any.
- Corresponding MCA parameter, if any.
- Notes, including alias behavior.

Add short cross-references from the MPI file info man pages to the
MPI-IO tuning page:

- `docs/man-openmpi/man3/MPI_File_open.3.rst`
- `docs/man-openmpi/man3/MPI_File_set_info.3.rst`
- `docs/man-openmpi/man3/MPI_File_get_info.3.rst`

## Test Plan

Add a standalone MPI test under `test/simple`. Do not wire it into
`make check`; Open MPI's `make check` does not run `mpirun`-launched
tests.

The test should be manually runnable, for example with:

```sh
mpirun -n 2 --mca io ompio ./ompio_file_info
```

Suggested coverage:

- Open with `MPI_INFO_NULL`, call `MPI_File_get_info`, and verify
  supported OMPIO defaulted hints are returned.
- Open with a supported hint such as `cb_buffer_size`, and verify the
  returned value.
- Open with an unknown key, and verify it is not returned.
- Verify accepted aliases preserve user spelling where the selected
  component supports the alias.
- Call `MPI_File_set_info` with a mutable supported hint, and verify
  `MPI_File_get_info` reflects the update.
- Call `MPI_File_set_info` with an unknown key, and verify previous or
  default hints remain unchanged.
- Call `MPI_File_set_view` with view-related accepted hints, and verify
  `MPI_File_get_info` reflects accepted changes.

Filesystem-specific cases should skip gracefully if the relevant
component or filesystem is not active.

## Risks

The `opal_infosubscribe_subscribe()` callback returns `const char *`,
which is convenient for static strings but awkward for dynamically
formatted values. If a component needs to publish a dynamically
computed implementation hint, it should set that value directly in
`fh->super.s_info` through an OMPIO helper using `opal_info_set()`.

The main ordering risk is applying user info before all relevant
subcomponents have registered their subscriptions. OMPIO should ensure
subscriptions are installed before reapplying open-time user info.
