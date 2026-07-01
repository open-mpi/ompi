# MPI-5 ABI support

**Last updated:** 29 June 2026

> **For AI agents:** Start with [`../../AGENTS.md`](../../AGENTS.md) for Open MPI
> development orientation. This document describes the **MPI ABI
> implementation** — how Open MPI supports the standardized
> Application Binary Interface defined in MPI-5 Chapter 20.

**Key terms:**
- **MPI ABI** — the *standardized* binary interface (MPI-5 Chapter 20,
  Appendix A) that enables dynamic library interoperability across
  implementations (MPICH, Open MPI, etc.).
- **Open MPI ABI** (called **OMPI ABI** in this document for brevity) —
  Open MPI's traditional, implementation-specific ABI used by
  `libmpi.so` and Open MPI's `mpicc` wrapper.

---

## Overview: what the MPI ABI enables

By supporting a standardized ABI, an MPI application can be **compiled
once** against one implementation (e.g., MPICH) and **run** with a
different ABI-compliant implementation (e.g., Open MPI) at runtime — no
recompile required. This only applies to **dynamically linked**
applications.

**Library and wrapper names** (mandated by the MPI-5 standard):
- Library filename: `libmpi_abi.so`
- Compiler wrapper: `mpicc_abi`
- Standardized header: `mpi.h` (e.g., from
  <https://github.com/mpi-forum/mpi-abi-stubs>)

**Versioning:** Open MPI aims to follow the same standard-ABI library
naming / versioning convention used by other ABI implementations;
consult the top-level `VERSION` file for Open MPI's current SONAME
settings.

## Architecture: how Open MPI's libraries are structured

**Minimal internal changes:** Relatively few changes were made to Open
MPI's internal components to support the MPI ABI. The primary change is
in **library structure**: Open MPI now builds two top-level C MPI
interface libraries — one for the standardized MPI ABI and one for Open
MPI's traditional ABI — both linked against a shared internal
implementation library.

**Library structure:**

```
         MPI ABI                 OMPI ABI
  +----------------------+----------------------+
  |     libmpi_abi.la    |       libmpi.la      |
  +----------------------+----------------------+
  |              libopen_mpi.la                 |
  +---------------------------------------------+
```

- **`libmpi_abi.la`** — the standardized MPI ABI library, linked by
  `mpicc_abi`.
- **`libmpi.la`** — Open MPI's traditional (non-standard) ABI library,
  linked by `mpicc`. (Name configurable via `@OMPI_LIBMPI_NAME@` in
  Makefiles.)
- **`libopen_mpi.la`** — the shared internal implementation used by
  both C ABI layers.

**Fortran:** The Fortran OMPI libraries (e.g., `libmpi_usempi.so`) are
linked against `libmpi.so.0` (the OMPI ABI), **not** the MPI ABI.

## Installation layout

**Install directories:**
- **Libraries:** Both `libmpi_abi.so` and `libmpi.so` install to
  `<prefix>/lib` (no conflicts — different filenames).
- **Headers:**
  - OMPI ABI: `<prefix>/include/` (traditional location)
  - MPI ABI: `<prefix>/include/standard_abi/`
- **Wrappers:**
  - `mpicc` → links against `libmpi.so` (OMPI ABI)
  - `mpicc_abi` → links against `libmpi_abi.so` (MPI ABI), points to
    `standard_abi/mpi.h`

---

## Code generation: the binding infrastructure

Open MPI **generates** the MPI bindings at build time rather than
hand-writing them. The MPI ABI support reuses and extends the binding
generator originally developed for big-count support.

### Generating the ABI-compliant `mpi.h`

Instead of using the `mpi.h` from
<https://github.com/mpi-forum/mpi-abi-stubs>, Open MPI **generates** it
from the LaTeX source of the MPI-5 standard. This approach uncovered
bugs in both the standard itself and the `mpi-abi-stubs` repo.

**Four components:**

1. **`pympistandard`** (<https://github.com/mpi-forum/pympistandard>) —
   included in the `3rd-party/` folder.
2. **Two JSON files:**
   - `mpi-standard-apis.json` — generated as part of the MPI standard
     build process; also used to generate Makefile content.
   - `mpi-standard-abi.json` — generated via a tool (not yet upstream)
     that extracts standardized constant values (e.g., `MPI_COMM_WORLD`)
     from the Appendix A LaTeX.
   - Both live under `docs/` in the source tree; verify exact filenames
     and locations before editing.
3. **`c_header.py`** — parses the JSON files and generates the MPI ABI
   `mpi.h` and a **name-mangled** `abi.h` (used internally). Uses
   methods from `pympistandard`. (Eventually will be fully integrated
   into the binding infrastructure.)
4. **`abi.h.in` template** — located in `ompi/mpi/c/`; structures the
   layout of the generated headers (not the content itself).

### Generating the C bindings

The binding infrastructure uses **template files** (`*.c.in`) to
generate **up to four variants** of each MPI function:

1. OMPI ABI, int count
2. OMPI ABI, big count
3. MPI ABI, int count
4. MPI ABI, big count

**Special templates:**
- `*.c.in_obc` — big-count-only functions
- `*.c.in_nbc` — int-count-only functions

**Exclusions:** Some functions are **not present** in the MPI ABI (e.g.,
all `_f2c`/`_c2f` conversion routines), and there are restrictions on
which deprecated procedures are included. These are managed via
`ompi/mpi/c/Makefile.am` and `ompi/mpi/c/Makefile_abi.include`.

**OMPI ABI generation:** Simple — only involves code changes for
big-count support.

**MPI ABI generation:** More complex. Generated source files include
**both** ABIs' headers (`mpi.h` and `abi.h`), so each file has **dual
definitions** of MPI constants:
- `MPI_COMM_WORLD` — OMPI ABI value (address of a global variable)
- `MPI_COMM_WORLD_ABI_INTERNAL` — MPI ABI value (an integer)

**Conversion logic:** The binding framework:
1. Parses input argument types.
2. Generates calls to **converter methods** to translate MPI ABI
   constants → OMPI ABI constants.
3. Calls a **wrapped version** of the original function.
4. Converts output arguments (OMPI ABI → MPI ABI), including
   `MPI_Status` fields and error return values.

**Generated helpers:** The framework also generates `abi_converters.h`
and `abi_converters.c` (in addition to the binding source files).

### Critical implications for agents

**Three key constraints from the conversion approach:**

1. **Minimal changes to internal Open MPI source code.** Most of the ABI
   support is in the generated bindings, not the core implementation.

2. **Do NOT call public `MPI_*()` entry points from the internal
   implementation.** The MPI ABI entry points expect standardized ABI
   handle / sentinel representations, not Open MPI's internal
   representations. Internal code must call the corresponding
   `ompi_*` routines instead (see
   [`../../AGENTS.md`](../../AGENTS.md) golden rules).

3. **Converter arrays for non-blocking/persistent operations.** In some
   non-blocking and persistent methods, arrays of converted values must
   be allocated and freed later (upon completion of the non-blocking
   collective or release of the persistent request).

---

## User-defined functions

The MPI standard specifies several kinds of **user-provided callback
functions**. Each requires different handling in the MPI ABI:

| Function type | MPI ABI handling |
|---------------|------------------|
| Attribute copy/delete | **Wrapper generation:** wrappers convert OMPI ABI ↔ MPI ABI constants; no changes to OMPI internals. |
| Operator functions (reductions) | **Extended internal support:** translation routine converts datatype arguments OMPI ABI → MPI ABI. |
| Error handlers | **Extended internal support:** similar to operator functions. |
| Generalized request functions | **No additional wrapper support needed:** `MPI_Status` structure is compatible enough in both ABIs. |
| Datarep functions | **No conversion handling needed:** Open MPI's current datarep support does not require ABI-specific handling. |
| MPI_T event callbacks | **No support needed:** MPI_T event implementation is just stubs. |

---

## Editing guidelines for developers and AI agents

When working on ABI-related code, remember:

**DO NOT hand-edit:**
- Generated bindings (`.c` files emitted from `.c.in` templates) — edit
  the **template** or the **generator** instead.
- `abi_converters.h` / `abi_converters.c` — regenerate via the binding
  infrastructure.
- The generated `mpi.h` and `abi.h` — edit the `abi.h.in` template or
  the JSON files / `c_header.py` generator.
- Generated test probes emitted into the build tree by `test/mpi-abi/`.

**DO edit (with care):**
- Templates under `ompi/mpi/c/` (`.c.in`, `.c.in_obc`, `.c.in_nbc`)
- The `c_header.py` generator or `abi.h.in` template
- `ompi/mpi/c/Makefile.am` or `Makefile_abi.include` (for build-system
  changes)
- JSON files in `docs/` (if fixing data from the standard)

**After modifying build-system files or templates:**
1. Regenerate: `./autogen.pl`
2. Reconfigure: `./configure <same options>`
3. Rebuild: `make -j`
4. Test: see the MPI ABI test suite section below

**Test both ABIs:**
- OMPI ABI: `mpicc` + `mpirun`
- MPI ABI: `mpicc_abi` + `mpirun` (verify in `<prefix>/include/standard_abi/`)

---

## MPI ABI test suite

Open MPI includes a dedicated test suite under `test/mpi-abi/` that
validates the ABI-facing surface. The suite is a Python program
(`mpi_abi_tests.py`) that checks metadata consistency, installed
artifacts, symbol reachability, handle translation, complete public API
call paths, callback conversion, and cross-implementation compatibility.

**Important:** These tests validate the **ABI layer** (the translation
between MPI ABI and OMPI ABI), not the underlying MPI algorithms. They
assume the core Open MPI implementation is already tested by the general
MPI test suite.

### Three make targets

#### 1. `make check` — Fast metadata and manifest checks

Runs automatically as part of the standard `make check` (via
`check-local` in `test/mpi-abi/`).

- **Prerequisites:** None — runs entirely from source and build trees
- **No installation required:** Does not need `mpicc_abi`, `mpirun`, or
  an installed Open MPI
- **What it checks:**
  - Compares MPI-standard-derived ABI metadata (from `docs/`) against
    the test suite's manifest
  - Validates classification rules, generated-source contracts, C header
    constants, and Fortran helper contracts
  - Catches drift between the ABI description and what's implemented,
    skipped, or uncovered **before** installation or launching
- **Output:** `$(builddir)/test/mpi-abi/check-results/`

**When to run:** Always safe; runs in CI as part of normal `make check`.
Even when changing runtime ABI code, this mode remains useful because it
catches metadata / manifest drift independently of installation and
launcher setup.

#### 2. `make check-abi` — Installed standard ABI checks

Validates an **installed** Open MPI against the standard MPI ABI.

- **Prerequisites:**
  - Open MPI must be installed (`make install`)
  - `mpicc_abi`, `mpirun`, and the standard ABI header must be
    discoverable by the test runner
  - Tool and path discovery can be overridden with `OMPI_ABI_TEST_*`
    environment variables; see `test/mpi-abi/Makefile.am` and the
    runner sources for the authoritative list
- **What it checks:**
  - Compiles and runs test probes using the installed `mpicc_abi` and
    standard ABI header
  - Validates handle translation, API call paths, and callback
    conversion
  - Exercises the ABI layer end-to-end
- **Output:** `$(builddir)/test/mpi-abi/check-abi-results/`

**When to run:** After `make install`, when validating the installed ABI
layer.

#### 3. `make check-abi-mpich` — Cross-implementation compatibility

Tests **interoperability** between Open MPI's `libmpi_abi.so` and
MPICH's standard ABI artifacts.

- **Prerequisites:**
  - Open MPI must be installed (`make install`)
  - **MPICH** (or another ABI-compliant implementation) must be installed
    with its `mpicc_abi` on `PATH`
  - Open MPI's `mpirun` must be discoverable by the test runner
  - Tool and path discovery can be overridden with `OMPI_ABI_TEST_*`
    environment variables; see `test/mpi-abi/Makefile.am` and the
    runner sources for the authoritative list
- **What it checks:**
  - Compiles test probes using **MPICH's `mpicc_abi`** and header
  - Links and runs against **Open MPI's `libmpi_abi.so`**
  - Validates that an application built with one implementation can run
    with another (the **core ABI promise**)
- **Output:** `$(builddir)/test/mpi-abi/check-abi-mpich-results/`

**When to run:** When validating cross-implementation compatibility (the
defining feature of the standard ABI). This mode is intended to confirm
the standard ABI promise: compile with another implementation's
ABI-facing wrapper and header, then run against Open MPI's ABI library
and launcher environment.

### Running the test suite manually

The suite can also be invoked directly:

```bash
cd test/mpi-abi
python -B mpi_abi_tests.py --srcdir=<top-srcdir> --builddir=<top-builddir> <mode>
```

**Modes:**
- `manifest` — Print the test manifest (what's covered, skipped, etc.)
- `coverage` — Print coverage statistics
- `check-fast` — Run fast metadata/manifest checks (same as `make check`)
- `check-abi` — Run installed ABI checks (same as `make check-abi`)
- `check-abi-mpich` — Run cross-implementation checks (same as `make
  check-abi-mpich`)

**Options:**
- `--complete-gate` — Fail if implemented ABI entries lack test coverage
- `--no-progress` — Suppress per-check progress lines
- `--color-tests={auto,yes,no,always,never}` — Control colorized output

### For AI agents: when to run which test

| Scenario | Test target | Why |
|----------|-------------|-----|
| Modified binding templates (`.c.in`) | `make check` | Fast check for metadata consistency |
| Modified `abi_converters.*` or ABI constants | `make check-abi` | Validate handle translation and conversion logic |
| Modified `abi.h.in` or `c_header.py` | `make check-abi` | Validate generated header correctness |
| Modified JSON files in `docs/` | `make check` first, then `make check-abi` | Catch metadata drift, then validate runtime behavior |
| Before submitting a PR touching ABI code | Prefer `make check`, then `make check-abi`, and `make check-abi-mpich` when MPICH is available | Best practical validation coverage |

**Note:** `make check-abi-mpich` requires a separate MPICH installation
and is not always practical in all environments. It is primarily for
validating cross-implementation compatibility claims.

---

See [`../../AGENTS.md`](../../AGENTS.md) for general Open MPI development
practices, coding standards, and contribution workflow.
