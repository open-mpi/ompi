# AGENTS.md — Open MPI extensions (`ompi/mpiext/`)

Scoped guidance for AI coding agents (and humans) working on the MPI
*extensions* tree. This complements the top-level
[`AGENTS.md`](../../CLAUDE.md) / `CLAUDE.md`; when the two disagree, the
top-level file and the docs under [`docs/`](../../docs/) win.

An MPI extension is a self-contained set of non-standard, Open
MPI-specific APIs and/or constants exposed to user applications via the
separate `mpi-ext.h` header (and the Fortran `mpif-ext.h` /
`use mpi_ext` / `use mpi_f08_ext` equivalents). Each subdirectory here
is one extension; its name is its directory name. See
[`example/README.md`](example/README.md) for the canonical walk-through
of an extension's required layout, and [`README.md`](README.md) for the
symbol-naming rules (`OMPI_` vs. `MPIX_` vs. `MPI_`).

## Current extensions and where each is documented

The canonical, user-facing *index* of all extensions is
[`docs/features/extensions.rst`](../../docs/features/extensions.rst)
(rendered under "Open MPI-specific features → Open MPI extensions").
**Every buildable extension must be listed there.**

## Documentation-sync checklist

When you **add, remove, or change the public API/constants of an
extension** in this tree, update the docs in the same PR:

1. **`docs/features/extensions.rst`** — the "Available extensions" list.
   Add/remove/adjust the bullet so it matches what actually builds. This
   is the single most-often-missed step.
2. **Man pages** — every public `MPIX_*` / `OMPI_*` symbol should have a
   page under [`docs/man-openmpi/man3/`](../../docs/man-openmpi/man3/),
   listed in that directory's `index.rst`. New API → new man page.
3. **Accelerator extensions** (`cuda`, `rocm`) — keep the relevant
   `docs/tuning-apps/accelerators/*.rst` page in sync.
4. **`ftmpi`** — user-facing behavior is documented in
   [`docs/features/ulfm.rst`](../../docs/features/ulfm.rst), not just the
   man pages.
5. **Changelog** — add a release-notes entry under
   [`docs/release-notes/changelog/`](../../docs/release-notes/changelog/)
   (`vMAJOR.MINOR.x.rst`) for any user-visible change.
6. **LLM-friendly docs** — if you touch public MPI-extension
   documentation, check whether the curated sources under
   [`docs/llms-src/`](../../docs/llms-src/) (e.g. the interface guide)
   need a matching update; see
   [`docs/developers/llm-friendly-docs.rst`](../../docs/developers/llm-friendly-docs.rst).

## How an extension is selected to be built

Each extension has a mandatory `configure.m4` whose macro decides,
during Open MPI's `configure`, whether the extension builds. Extensions
are enabled by default and can be disabled globally with
`--disable-mpi-ext` (or selectively via its argument list). Individual
extensions typically also gate themselves on a prerequisite — e.g.
`cuda`/`rocm` on the corresponding accelerator support being configured,
`shortfloat` on the compiler actually providing `_Float16` / `short
float`. When you document an extension, describe *when* it is present,
not just what it does — that build-selection story is currently
under-documented.
