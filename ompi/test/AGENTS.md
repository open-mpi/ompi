# AGENTS.md — OMPI-layer tests (`ompi/test/`)

Scoped guidance for AI coding agents (and humans) working on tests under
`ompi/test/`. This complements the top-level [`AGENTS.md`](../../AGENTS.md);
when the two disagree, the top-level file and the docs under
[`docs/`](../../docs/) win.

**This is not the only test tree.** The top-level [`test/`](../../test/)
directory holds OPAL-layer tests (they link only `libopen-pal`, no MPI
API). `ompi/test/` holds OMPI-layer tests: they may call `MPI_*` APIs,
link `libmpi`/`libopen_mpi`, and exercise the MPI implementation itself.
If you're looking for MPI-facing test coverage, you want this directory.

## Orientation: what's here and how each piece runs

`ompi/test/` is not one uniform test suite — it's a `SUBDIRS` collection
(see [`Makefile.am`](Makefile.am)) of independently-built directories
with different scopes, different run mechanisms, and in one case
(`mpi-abi/`) a whole separate Python test framework. Before adding a
test, find the directory that already matches its scope rather than
inventing a new one.

| Directory | Scope | Run mechanism |
|---|---|---|
| [`t/`](t/) | MPI_T tool-interface tests (control vars, perf vars, events, sessions/world lifecycle). Singleton `MPI_Init`/`MPI_T_init_thread`; no launcher. | `make check` (Automake `TESTS`) |
| [`file/`](file/) | MPI_File info-hint reporting (ompio), on `MPI_COMM_SELF`. | `make check` |
| [`datatype/`](datatype/) | OMPI datatype engine (`ompi/datatype`) + `MPI_Type_*` API, single process. | `make check` |
| [`part/`](part/) | Partitioned communication (`MPI_P*`) on `MPI_COMM_SELF` / `MPI_PROC_NULL`. | `make check` |
| [`general/`](general/) | Catch-all: one test program per OMPI-layer module not covered above (communicator, group, win, errhandler, info, instance, request, proc, message, op, attribute, interlanguage, ...). Single process; see its own [`README.md`](general/README.md) for the init styles, coverage table, and known `np>1` gaps. | `make check` |
| [`monitoring/`](monitoring/), [`spc/`](spc/) | Need multiple processes. | **NOT** run by `make check` — `noinst_PROGRAMS`, built only; run by hand / CI script |
| [`reexport/`](reexport/) | Symbol resolution between `libmpi` and `libopen_mpi`. | `make check` |
| [`bindings-generator/`](bindings-generator/) | Pure-Python unit tests for the MPI binding **generator** (`ompi/mpi/bindings/`) — no compiler, no MPI, no build required. | `make check` |
| [`mpi-abi/`](mpi-abi/) | The MPI Standard ABI layer: header, wrapper, converters, and runtime behavior reached through it. Its own Python-driven framework — see below. | `make check` runs only its **fast** metadata checks; `make check-abi` / `make check-abi-mpich` run the full **installed** suite (needs `make install` first) |
| [`mpirun/`](mpirun/) | Behavior that only exists between ranks of a real launched job (e.g. wire-up). | `make check-mpirun`, **after** `make install`, with the install's `bin/` on `PATH` |

Notes that generalize across the simple (Automake `TESTS`) directories:

- Most tests that call `MPI_Init()`/`opal_init()` include
  [`Makefile.mca-dso-check`](../../Makefile.mca-dso-check) (see the
  comment block at its top) so `make check` still works in an
  uninstalled `--enable-mca-dso` build. Copy that `include` line into
  any new `Makefile.am` whose test calls `MPI_Init`.
- Tests that call `MPI_*` APIs (link `libmpi`) usually also reference
  internal OMPI symbols pulled in by macro expansion (predefined
  datatypes like `ompi_mpi_int`, etc.), which live in `libopen_mpi`.
  `libopen_mpi` is a link-time dependency of `libmpi`, but the linker
  does not always pull it in transitively in an uninstalled tree — so
  every `*_LDADD` in this tree explicitly lists `libmpi`,
  `libopen_mpi`, **and** `libopen-pal`, in that order. Copy this
  pattern (`$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la
  $(top_builddir)/ompi/libopen_mpi.la
  $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la`) rather than reinventing it.
- Editing an existing `Makefile.am` (adding a test program, a source
  file, an `EXTRA_DIST` entry) needs only a plain `make` — it
  regenerates `Makefile.in`/`Makefile` on its own. You only need
  `./autogen.pl` + reconfigure if you add a *new* directory that must
  be registered in `configure.ac`/`config/*.m4`. (Same rule as the
  top-level `AGENTS.md`; repeated here because it is the single most
  common thing you'll do in this tree.)
- Multi-process (`np>1`) MPI behavior is largely out of scope for the
  plain `make check` directories (`t/`, `file/`, `datatype/`,
  `general/`, `part/`) by design — they are single-process/singleton so
  they need no launcher. `monitoring/`, `spc/`, and `mpirun/` exist
  specifically to hold the multi-process cases; don't add an `np>1`
  test to a single-process directory's `TESTS`, and don't expect
  `make check` to run it if you do.

## `mpi-abi/`: read this before touching it

This is the subtree most likely to cost you time if you haven't seen
it before — it is not a set of individual C test programs; it's a
Python framework that *generates and drives* C probes. Start with
[`mpi-abi/README.md`](mpi-abi/README.md) for the full picture (test
groups, environment-variable knobs, MPICH cross-testing); this section
is the fast orientation.

**The pieces:**

- `mpi_abi_tests.py` is the driver invoked by `make check` (fast path)
  / `make check-abi` / `make check-abi-mpich` (installed path).
- `_abi_*.py` modules hold the actual check logic (metadata/manifest
  checks, installed-artifact checks, probe generation and execution,
  cross-implementation checks). `_abi_tables.py` holds the *data*: the
  static case tables that describe each installed C probe, and
  callback API-name sets.
- `cases/c-abi/`, `cases/c-runtime/`, `cases/c-callback/` hold the
  actual test *content* as small source fragments (`*.cbody.in`, and
  optionally `*.prologue.in`), **not** full C files.
- `templates/c_probe.c.in` is the one shared C skeleton: it has
  `@PROLOGUE@` and `@BODY@` placeholders and a `main(argc, argv)`.

**How one probe becomes a running test:**

1. A dict entry is added to a case list in `_abi_tables.py` (e.g.
   `INSTALLED_C_ABI_PROBES`), naming the probe and pointing at its
   `body_file` (required) and, if it needs file-scope helpers, its
   `prologue_file` (optional).
2. `body_file` (`*.cbody.in`) is straight-line C **inserted inside
   `main()`** — write it at column-zero indentation; the generator
   re-indents it. It must not contain top-level declarations (no
   `static` helper functions, no file-scope globals) — those go in the
   prologue instead, because they cannot legally appear inside a
   function body.
3. `prologue_file` (`*.prologue.in`), when present, is inserted
   **before `main()`** — this is where callback functions and their
   backing file-scope state (e.g. a fired-counter a callback
   increments) belong. Not every probe needs one; only add it when the
   body needs a named callback or file-scope helper.
4. Both files are `EXTRA_DIST`ed by name in `Makefile.am`, under the
   list matching their `cases/` subdirectory (`C_ABI_CASES` /
   `C_RUNTIME_CASES` / `C_CALLBACK_CASES`). **A new `.cbody.in` or
   `.prologue.in` file that isn't added there will build and run fine
   locally but will be silently missing from a distribution tarball.**
5. Optional table keys worth knowing before you need to grep for them:
   `rank_count` (canonical launch size — probes hard-code buffer
   sizes/expectations for exactly this count), `requires_feature` (skip
   cleanly if a build-time feature like `rma`, `mpi_io`, `mpit_events`,
   `dynamic_process` isn't enabled), `requires_fortran`, and
   `skip_exit_codes` (map a probe's documented "feature unavailable"
   exit code, e.g. `77`, to a named skip reason so the runner records a
   skip instead of a failure).

**Running it while developing**, before wiring anything into the
Makefile:

```sh
# Pure-Python unit tests for the runner itself -- fast, no MPI, no build.
python3 -B ompi/test/mpi-abi/test_abi_units.py

# Render one probe's generated C source without running the full suite,
# to sanity-check a new .cbody.in/.prologue.in pair:
python3 -B -c "
import sys; sys.path.insert(0, 'ompi/test/mpi-abi')
from pathlib import Path
from _abi_common import _probe_body_text
from _abi_installed import _c_probe_source
from _abi_tables import INSTALLED_C_ABI_PROBES
case = next(c for c in INSTALLED_C_ABI_PROBES if c['name'] == 'YOUR_PROBE_NAME')
print(_c_probe_source(Path('.'), case, _probe_body_text(Path('.'), case)))
"
```

The full installed suite needs Open MPI built **and installed** first
(`make install`), plus that install's `bin/` on `PATH` so `mpicc_abi`
and `mpirun` resolve:

```sh
make install
make -C ompi/test/mpi-abi check-abi        # exercises the standard ABI
make -C ompi/test/mpi-abi check-abi-mpich  # cross-checks against MPICH, if available
```

Reports land in mode-specific directories (`check-results/`,
`check-abi-results/`, `check-abi-mpich-results/`); these are checked-in
fixtures that get overwritten by each run, not something to hand-edit.

## Adding a test: quick decision guide

- **Single-process, exercises one OMPI object/module, no launcher
  needed** → add a `.c` under the matching directory (or `general/` if
  none matches) and wire it into that directory's `check_PROGRAMS` /
  `TESTS` in `Makefile.am`, following the `_SOURCES`/`_LDFLAGS`/`_LDADD`
  pattern already used by its neighbors. Plain `make` picks up a
  `Makefile.am` edit.
- **Needs the MPI Standard ABI specifically** (an ABI converter, a
  standard-ABI-only code path, an installed `mpicc_abi`/`mpirun`
  runtime probe) → add a case under `mpi-abi/` per the section above,
  not a standalone `.c` file elsewhere.
- **Needs `np>1` but can run without a real launcher** (rare; most
  `np>1` behavior needs `mpirun`) → look at whether `monitoring/` or
  `spc/`'s `noinst_PROGRAMS` pattern fits; remember these are *not* run
  by `make check`.
- **Genuinely needs a launcher** (endpoint wire-up, anything that only
  differs between two real ranks of a job) → add it to `mpirun/`,
  following its existing ground rules (≤2 ranks, always pass
  `--timeout`) — and remember it only runs via `make -C
  ompi/test/mpirun check-mpirun` after `make install`, never via plain
  `make check`.
- **Testing the binding *generator* itself** (not generated bindings,
  the Python code in `ompi/mpi/bindings/` that emits them) → add a
  `unittest.TestCase` to
  `bindings-generator/test_bindings_generator.py`, not a C test.

