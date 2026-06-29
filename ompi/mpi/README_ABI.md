# MPI-5 ABI support

**Last updated:** 29 June 2026

> **For AI agents:** Start with [`/AGENTS.md`](/AGENTS.md) for Open MPI
> development orientation. This document describes the **MPI ABI
> implementation** — how Open MPI supports the standardized
> Application Binary Interface defined in MPI-5 Chapter 20.

**Key terms:**
- **MPI ABI** — the *standardized* binary interface (MPI-5 Chapter 20,
  Appendix A) that enables dynamic library interoperability across
  implementations (MPICH, Open MPI, etc.).
- **OMPI ABI** — Open MPI's *traditional, non-standard* ABI used by
  `libmpi.so` and Open MPI's `mpicc` wrapper.

---

## Overview: what the MPI ABI enables

By supporting a standardized ABI, an MPI application can be **compiled
once** against one implementation (e.g., MPICH) and **run** with a
different ABI-compliant implementation (e.g., Open MPI) at runtime — no
recompile required. This only applies to **dynamically linked**
applications.

**Library and wrapper names** (mandated by the MPI-5 standard):
- Library: `libmpi_abi.so` (SONAME: `libmpi_abi.0`; must match MPICH)
- Compiler wrapper: `mpicc_abi`
- Standardized header: `mpi.h` (e.g., from
  <https://github.com/mpi-forum/mpi-abi-stubs>)

**Version control:** The SONAME major/minor versions are set in the
top-level `VERSION` file.

## Architecture: how Open MPI's libraries are structured

**Minimal internal changes:** Relatively few changes were made to Open
MPI's internal components to support the MPI ABI. The primary change is
in **library structure**: Open MPI now ships *two* top-level MPI
libraries — one for each ABI — both linked against a shared internal
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
- **`libopen_mpi.la`** — the shared internal implementation that both
  ABIs link against; contains all the core MPI logic.

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
   - Both reside in the top-level `docs/` folder.
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

**Generated helpers:** The framework also generates `abi_converter.h`
and `abi_converter.c` (in addition to the binding source files).

### Critical implications for agents

**Three key constraints from the conversion approach:**

1. **Minimal changes to internal Open MPI source code.** Most of the ABI
   support is in the generated bindings, not the core implementation.

2. **Do NOT call top-level `MPI_*()` functions from within
   `libopen_mpi`.** The top-level MPI ABI C entry points expect MPI ABI
   constants. Calling them from internal Open MPI code (which uses OMPI
   ABI constants) will fail. Always call the internal `ompi_*` routines
   instead (see [`/AGENTS.md`](/AGENTS.md) golden rules).

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
| Generalized request functions | **No conversion needed:** `MPI_Status` structure is similar enough in both ABIs. |
| Datarep functions | **No support needed:** Open MPI doesn't fully support datarep functions yet. |
| MPI_T event callbacks | **No support needed:** MPI_T event implementation is just stubs. |

---

## For AI agents: editing guidelines

When working on ABI-related code, remember:

**DO NOT hand-edit:**
- Generated bindings (`.c` files emitted from `.c.in` templates) — edit
  the **template** or the **generator** instead.
- `abi_converter.h` / `abi_converter.c` — regenerate via the binding
  infrastructure.
- The generated `mpi.h` and `abi.h` — edit the `abi.h.in` template or
  the JSON files / `c_header.py` generator.

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
4. Test: `make check` and smoke-test examples (see [`/AGENTS.md`](/AGENTS.md))

**Test both ABIs:**
- OMPI ABI: `mpicc` + `mpirun`
- MPI ABI: `mpicc_abi` + `mpirun` (verify in `<prefix>/include/standard_abi/`)

See [`/AGENTS.md`](/AGENTS.md) for general Open MPI development
practices, coding standards, and contribution workflow.
