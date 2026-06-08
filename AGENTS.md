<!--
  Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.

  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# AGENTS.md

Guidance for AI coding agents (and the humans driving them) working in
the Open MPI source tree. This file is an *orientation map*, not the
full rulebook: the authoritative, human-maintained documentation lives
under [`docs/developers/`](docs/developers/) and
[`docs/contributing.rst`](docs/contributing.rst) (rendered at
<https://docs.open-mpi.org/>). When this file and those docs disagree,
**the docs win** — and please fix this file.

AI-assisted contributions are welcome. But Open MPI runs on the largest
supercomputers in the world and across a huge range of operating
systems and hardware. We want careful, portable, performant code — not
plausible-looking code that solves one problem in one environment at the
expense of others. Hold yourself to the same bar as a thoughtful human
contributor.

## What Open MPI is

Open MPI is an open source implementation of the [Message Passing
Interface (MPI) specification](https://www.mpi-forum.org/docs/) — a
high-level library for sending discrete, typed messages between
processes, independent of the underlying network or OS. It also
includes a run-time system that launches and manages the lifecycle of
many processes across many hosts as a single MPI "job".

## The mental model: three projects

The code base is divided into three *projects*, which are strict
abstraction barriers — each compiles to its own library with a one-way
dependency order:

```
OSHMEM  (liboshmem)   OpenSHMEM API layer
   │  depends on
OMPI    (libmpi)      MPI API layer + language bindings
   │  depends on
OPAL    (libopen-pal) portability layer (OS/arch abstractions)
```

- **OPAL** — portability primitives. Symbols prefixed `opal_` / `OPAL_`.
  This is where most OS/arch `#if` blocks belong.
- **OMPI** — everything the MPI standard mandates: the language bindings
  (C, several Fortran flavors, non-standard Java) on top, MCA frameworks
  underneath. Symbols prefixed `ompi_` / `OMPI_`; only *official* MPI
  symbols get `MPI_` / `mpi_`.
- **OSHMEM** — the OpenSHMEM API layer; sibling to OMPI, changes slowly.
  Symbols prefixed `oshmem_` / `OSHMEM_`.

**Linker boundary (a real, hard error if you violate it):** code in a
lower layer *cannot* directly call functions in a higher layer. OPAL
cannot call OMPI or OSHMEM; OMPI cannot call OSHMEM. The legal way for a
lower layer to reach upward is a **callback function pointer** handed
down from the higher layer. Direct upward calls fail to resolve at link
time.

## MCA: the Modular Component Architecture

OPAL, OMPI, and OSHMEM are built almost entirely out of MCA plugins.
Read [`docs/developers/terminology.rst`](docs/developers/terminology.rst)
and the MCA section it points to before doing real work. The hierarchy:

- **Project** → **Framework** → **Component** → **Module** (runtime
  instance, like a C++ object). Each level is isolated from its
  siblings; a framework exposes a top-level header for its public API.
- Example: `opal/mca/btl/` is the BTL framework in OPAL;
  `opal/mca/btl/tcp/` is one component within it.
- **MCA parameters** let users change behavior at run time (env var,
  file, CLI). **Prefer adding an MCA parameter over hard-coding a
  constant** — this is idiomatic and expected here.

## Golden rules (the things agents most often get wrong)

From [`docs/developers/source-code.rst`](docs/developers/source-code.rst)
and [`docs/contributing.rst`](docs/contributing.rst):

- **Prefix rule.** Filenames are prefixed `<framework>_<component>`.
  Public symbols in a component are prefixed
  `<project>_<framework>_<component>` (`<project>` ∈ `mca`, `opal`,
  `ompi`, `oshmem`). Non-public symbols must be `static` or otherwise
  kept out of global scope. When in doubt, add the prefix.
- **Include `<level>_config.h` first** — `opal_config.h`,
  `ompi_config.h`, or `oshmem_config.h` for the layer you're in — as the
  very first `#include`, before any system header.
- **MPI back-end code must never call public `MPI_*()` APIs.** The
  bindings are thin wrappers; call the internal `ompi_*` routines, not
  the user-facing entry points.
- **New files need the standard copyright/license header.** Copy the
  multi-institution BSD header block — including the `$COPYRIGHT$` and
  `$HEADER$` tokens — from a neighboring file. If you substantially
  change an existing file, add your copyright line to its block.
- **`#define` logical macros to `0` or `1`; never `#undef` them.** Test
  with `#if FOO`, not `#ifdef FOO`, so a misspelling is a compiler
  error, not a silent false.
- **Put constants on the left** of equality tests: `if (NULL == ptr)`.
- **Always brace blocks**, even one-liners. **4-space indents, never a
  literal tab character**, in any language.
- C11 is required (Open MPI ≥ 6.0): C++-style `//` comments and C99
  mixed declarations are allowed and preferred. Fortran has no formal
  style — match the surrounding code.
- **Stay compiler-warning-free.** Open MPI strives to build with zero
  compiler warnings. Do not introduce code that adds new warnings.

## Generated code: edit the source, not the output

The MPI C/Fortran bindings are **generated at build time** by the Python
generator under [`ompi/mpi/bindings/`](ompi/mpi/bindings/)
(`bindings.py` + `ompi_bindings/`), driven in part by official MPI
symbol/signature data pulled from the MPI Forum's `pympistandard`. If
you need to change a binding's behavior, change the **generator,
templates, or the back-end implementation** — never the emitted `.c`/`.h`
files.

## Do NOT hand-edit

- **`3rd-party/` and the git submodules** (embedded OpenPMIx, and the
  Open MPI *fork* of PRRTE). Fixes belong upstream, not patched in here.
- **Autotools-generated output** — `configure`, `Makefile.in`,
  `config.status`, anything produced by `./autogen.pl`. Edit
  `configure.ac`, `Makefile.am`, or the m4 in [`config/`](config/)
  instead.
- **Generated MPI bindings** — see the section above.
- **Pre-rendered docs** — shipped HTML and generated man pages. Edit the
  RST sources under [`docs/`](docs/).

## Build and test

Open MPI uses the GNU Autotools (Autoconf / Automake / Libtool). From a
Git clone:

```sh
./autogen.pl            # regenerate the build system (one-time / after build-system changes)
./configure --prefix=/path/to/install
make -j                 # full builds are SLOW
make install
```

Out-of-tree (VPATH) builds are not required, but they are often helpful
for sanity checks because they avoid perturbing the source tree:

```sh
mkdir build && cd build
../configure --prefix=/path/to/install
make -j
```

See [`docs/developers/building-open-mpi.rst`](docs/developers/building-open-mpi.rst)
and the [install docs](docs/installing-open-mpi/) for options.

## Modifying the configure / build system

Editing the build system means regenerating it — `make` alone can't,
and trying will wedge the tree. If you change `configure.ac` or any
`config/*.m4` file (including the embedded oac/Autotools macros), the
change does not take effect until the build system is regenerated. Do
not rely on a plain `make`: Open MPI builds in maintainer mode, so
`make` auto-triggers a partial in-tree Autotools regeneration that
frequently fails (e.g., unexpanded `OAC_*` macros, `config.status`
errors) and can leave the tree half-regenerated and
unbuildable. Instead, regenerate and reconfigure explicitly:

```sh
./autogen.pl
./configure <same options as the original configure>
make -j
```

Recover the original configure invocation options from the existing
tree with `./config.status --config` (or read the header of
`config.log`). This process is slow but mandatory after any
build-system source change — there is no safe shortcut.

Note that editing `Makefile.am` files do *not* require the full
`autogen.pl` + `./configure` process.  A simple `make` will regenerate
the relevant `Makefile[.in]` files and then complete the build
successfully.

**"Did I break it?" — layered:**

1. **Build cleanly.** A clean `make` after your change is the baseline.
   Open MPI is highly configurable at build time: many components,
   source files, directories, and generated artifacts are selected or
   omitted by `configure` and Automake based on the local environment.
   For any change to code or documentation that might be conditionally
   built, verify that your configured build is actually compiling or
   generating the thing you changed; do not assume this can be checked
   only by looking for `#if` blocks.
2. **Documentation-only changes can be narrower.** If the change is
   wholly under [`docs/`](docs/), it is often enough to configure with
   Sphinx support and run `make` in the `docs/` build directory instead
   of doing a full build, install, smoke test, or `make check`. Make
   sure Sphinx was really enabled by `configure`; one practical check is
   that `SPHINX_BUILD` in `config.status` names a valid executable.
3. **Quick smoke test.** After `make install`, put your `--prefix`'s
   `bin/` on your `PATH`, then build and run an example on the local
   host:

   ```sh
   cd examples && make        # compiles against the installed mpicc/mpifort wrappers
   mpirun --np 2 ./hello_c    # smallest launch + MPI_Init/Finalize sanity check
   mpirun --np 2 ./ring_c     # adds real point-to-point messaging
   ```
4. **Deeper validation** when your environment supports it: `make check`
   and the programs under [`test/`](test/). Be aware that the full suite
   and realistic MPI jobs frequently need a proper launcher and/or
   multiple hosts/specialized hardware — **do not assume you can run all
   of it locally, and don't report untested code as verified.**

**Add tests for new code.** Whenever practical, add unit tests under
[`test/`](test/) that are wired into `make check` (and therefore run in
CI). Prefer a `make check`-able test over a manual one-off so the
coverage sticks and regressions are caught automatically.

## Performance discipline

Performance is paramount: short-message **latency** and large-message
**bandwidth** are headline metrics, along with the ability to offload
work to networking/GPU hardware so the CPU can make progress
elsewhere. Microseconds — sometimes nanoseconds — matter, and much of
the hot path uses OS-bypass techniques talking directly to network,
CUDA, and ROCm hardware.

Concrete rules for hot paths:

- **Don't add allocations, locks, or branches to the critical
  send/receive path** without a clear, measured justification.
- **Guard debug output and expensive assertions behind
  `OPAL_ENABLE_DEBUG`** so release builds pay nothing for them.
- **Prefer an MCA parameter** to a hard-coded constant when a value
  might need tuning per environment.
- Keep environment-specific optimization where it belongs — generally in
  OPAL or in the hardware-specific component — not smeared across
  portable MPI logic.

## Contributing

Authoritative process:
[`docs/contributing.rst`](docs/contributing.rst). Highlights agents must
honor:

- **Sign off every commit.** Each commit needs a `Signed-off-by:` line
  per the Contributor's Declaration — use `git commit -s`. Commits
  without it are not accepted. This applies to AI-assisted work too: the
  human submitter certifies the contribution.
- **Commit messages:** a short first line saying *what* changed, then a
  body explaining *why*. Open MPI does **not** use Conventional Commits
  (`feat:`/`fix:` prefixes) — write prose. Don't add AI tooling
  attribution. Wrap commit-message lines at around 75 characters.
- **Branch flow:** land on `main` first via a GitHub pull request, then
  cherry-pick to the relevant release branch(es) `vMAJOR.MINOR.x` with a
  `(cherry picked from commit ...)` line at the end of the commit
  message; use `git cherry-pick -x` to add it. Never commit features
  directly to a release branch. See
  [`docs/developers/git-github.rst`](docs/developers/git-github.rst).
- **Update the docs and the changelog** when user-visible behavior
  changes: RST under [`docs/`](docs/), and a release-notes entry under
  [`docs/release-notes/changelog/`](docs/release-notes/changelog/)
  (`vMAJOR.MINOR.x.rst`).

## Repository map

| Path | What's there |
|------|--------------|
| `opal/` | OPAL portability layer (`opal/mca/` = its frameworks) |
| `ompi/` | OMPI / MPI layer; `ompi/mpi/` = language bindings, `ompi/mca/` = frameworks |
| `oshmem/` | OpenSHMEM layer |
| `3rd-party/` | embedded upstreams + submodules (OpenPMIx, PRRTE fork) — don't hand-edit |
| `config/` | m4 macros for Autoconf / Automake / Libtool |
| `docs/` | all RST documentation (Sphinx); `docs/developers/` is the dev guide |
| `examples/` | small MPI example programs (good smoke tests) |
| `test/` | unit / functional tests |
| `contrib/` | unsupported contributed scripts and tools |

## When in doubt

- Match the surrounding code's style and conventions — this is an old,
  multi-author code base with established patterns.
- Read the relevant [`docs/developers/`](docs/developers/) page before
  inventing a new pattern.
- Ask on the developer mailing list / a GitHub issue for anything large
  before writing it; see [`docs/contributing.rst`](docs/contributing.rst).
