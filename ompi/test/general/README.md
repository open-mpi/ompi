# General OMPI layer unit tests (`ompi/test/general/`)

Single-process unit tests for the OMPI layer (the top-level `ompi/*`
directories other than `mca`, `mpi`, `mpiext`, and `peruse`).  The tests
live entirely within the `ompi/` subtree and carry their own copy of the
test harness (`support.{c,h}`).

This is the catch-all directory: sibling directories under `ompi/test/`
are named for the single MPI object or interface that they exercise
(`t/` for MPI_T, `file/` for MPI_File info hints, `datatype/` for the
datatype engine), while `general/` covers everything else -- one test
program per OMPI-layer module.

## Running

From a configured build directory:

```sh
make -C ompi all                  # build libmpi (tests link against it)
make -C ompi/test check           # build + run all OMPI unit tests
make -C ompi/test/general check   # ...or just the general ones
```

Every test is a self-checking program that reports through the harness
(`test_init` / `test_verify` / `test_finalize`) and is wired into the
Automake `TESTS` list.  Note: the library is built with `-DNDEBUG`, so
`assert()` is a no-op in the tested code -- all verification goes through
`test_verify()`.

## Scope: single process only

By design these tests run in a single process (no `mpirun`/`mpiexec`).
They use one of two initialization styles:

- **partial init** -- `opal_init_util()` plus the relevant OMPI subsystem
  init (e.g. `ompi_mpiinfo_init()`); fast, no runtime bring-up.
- **full singleton `MPI_Init`** -- brings up the runtime in singleton
  mode (~0.1s, no launcher); used for anything needing live MPI state.

Multi-process (`np>1`) behavior is intentionally **out of scope here** and
is left to a separate follow-up effort.  The areas that require `np>1`
are catalogued below so that follow-up effort can target them directly.

## Coverage summary (single process, Linux gcov, line %)

| Directory | Coverage | Notes |
|-----------|---------:|-------|
| message     | 96% | |
| op          | 91% | |
| file        | 84% | full MPI-IO lifecycle on `MPI_COMM_SELF` (ompio) |
| class       | 84% | `ompi_seq_tracker` |
| win         | 79% | window lifecycle; RMA data movement is np>1 |
| interlib    | 78% | |
| attribute   | 82% | C + Fortran (mpif-h/use-mpi/use-mpi-f08) incl. C<->Fortran interlanguage |
| info        | 66% | |
| datatype    | 64% | constructors high; serialization cross-arch is np>1 |
| errhandler  | 56% | errcode 80-99%; fatal handlers + invoke need runtime |
| instance    | 50% | Sessions; PMIx-server paths are np>1 |
| proc        | 49% | proc table is populated by the np>1 modex |
| request     | 46% | generalized requests high; p2p completion is np>1 |
| group       | 39% | dense ops covered; sparse reps need large/np>1 groups |
| runtime     | 36% | init covered; finalize/error/SPC paths thin |
| communicator| 24% | most paths are collective/CID/FT, i.e. np>1 |
| dpm         |  3% | dynamic process management is inherently np>1 |
| debuggers   |  0% | TotalView DLL, driven by debugger callbacks |

`tools/` (mpirun, ompi_info, mpisync) are executables rather than a
library; `etc/`, `util/`, and `include/` contain no or header-only `.c`.

## Areas requiring `np>1` (for the follow-up effort)

The following code paths cannot execute in a single process and account
for most of the uncovered lines above.

### communicator (`ompi/communicator/`)
- `comm_cid.c` -- communicator-ID allocation is a collective negotiation
  over the group; only the singleton fast path runs with one rank.
- `comm.c` -- the collective-based creation paths (`ompi_comm_split`,
  `split_type`, `create`, `create_group`, inter-communicator merge/create)
  past the size-1 short-circuit; remote-group handling for
  inter-communicators.
- `comm_request.c` -- the nonblocking comm-creation state machine
  (`MPI_Comm_idup` and friends) requires progressing collectives.
- `comm_ft.c`, `comm_ft_detector.c`, `comm_ft_propagator.c`,
  `comm_ft_reliable_bcast.c`, `comm_ft_revoke.c` -- ULFM fault tolerance;
  entirely multi-rank (failure detection/propagation/revocation).

### proc (`ompi/proc/`)
- `ompi_proc_complete_init` and the proc-table population from the PMIx
  modex; `ompi_proc_for_name` on-demand creation of remote procs.  With
  one rank only the local proc exists.

### group (`ompi/group/`)
- `group_bitmap.c`, `group_strided.c`, `group_sporadic.c` -- the sparse
  group representations are only chosen for large (multi-rank) groups; a
  singleton always uses the dense representation.  Their
  translate/incl/excl/range implementations need a multi-rank parent.

### datatype (`ompi/datatype/`)
- `ompi_datatype_args.c` -- the cross-architecture branches of
  `__ompi_datatype_create_from_args` / pack-description (byte-swap and
  size conversion) only run when the remote proc has a different
  architecture.  The same-architecture round-trip *is* covered.
- `ompi_datatype_sndrcv.c` -- the internal send/recv helper needs an
  actual peer.

### request (`ompi/request/`)
- `req_wait.c`, `req_test.c` -- the point-to-point completion paths
  (waiting/testing on pending sends/receives) need a peer to make
  progress; only generalized-request and NULL-request paths run here.
- `req_ft.c` -- fault-tolerant request handling (multi-rank).

### instance (`ompi/instance/`)
- `MPI_Comm_create_from_group` and other operations that perform a
  PMIx-server-backed collective (CID allocation over a pset) fail with
  "PMIx server unreachable" in a launcher-less singleton.
- Multi-pset / dynamic-pset and PMIx-event-driven paths.

### errhandler (`ompi/errhandler/`)
- `errhandler_predefined.c` -- the `MPI_ERRORS_ARE_FATAL` /
  `MPI_ERRORS_ABORT` handlers call `ompi_mpi_abort()` and cannot be
  invoked in a passing test; only `MPI_ERRORS_RETURN` and custom handlers
  (via `*_call_errhandler`) are exercised.
- `errhandler_invoke.c` -- some object-type dispatch paths exercised only
  by real error conditions during communication.

### dpm (`ompi/dpm/`)
- Essentially all of `dpm.c`: `MPI_Comm_spawn[_multiple]`,
  `MPI_Comm_connect`/`accept`/`join`, and name publishing/lookup require
  a runtime with a launcher and/or multiple jobs.

### info (`ompi/info/`)
- `ompi_info_memkind_copy_or_set` and `..._validate` run during object
  creation only when `mpi_memory_alloc_kinds` is set (an MCA parameter);
  the parser (`ompi_info_memkind_process`) is covered directly.

### runtime (`ompi/runtime/`)
- Most of `ompi_mpi_init`/`finalize` is covered, but error/teardown
  branches, the SPC (software performance counter) reporting, and
  multi-threaded paths are not exercised single-process.

## Other (non-`np>1`) ceilings

- **MPI-IO in restricted/sandboxed environments**: the `file` test runs
  the full MPI-IO lifecycle and is expected to pass on Linux and macOS.
  In a restricted/sandboxed environment (e.g. some LLM coding harnesses),
  notably on macOS, it can fail at `MPI_File_open` because ompio's default
  shared-file-pointer component, `sm`, cannot create the POSIX named
  semaphore it needs (`sem_open` returns `EPERM`).  This is an environment
  limitation, not an Open MPI bug; the test fails (it does not skip) and
  prints a hint suggesting a re-run with `OMPI_MCA_sharedfp=^sm` to
  confirm the cause.
- **attribute (Fortran + interlanguage)**: `attribute.c` is at ~82%,
  covered by the C `attribute` test plus `attr_interlang`, a port of
  open-mpi/ompi-tests `simple/attr`.  `attr_interlang` is built in all
  three Fortran flavors (`attr_interlang_mpifh`, `_usempi`, `_usempif08`)
  from one template, and exhaustively exercises the 9 cases of setting an
  attribute in one language and reading it in another -- C, Fortran MPI-1
  (default INTEGER), and Fortran MPI-2 (address kind) -- for
  communicators, datatypes, and windows.  This covers the cross-language
  translation (`translate_to_fint`/`_aint`/`_c`).  The residual is
  error/out-of-resource/`abort()` paths only (not reachable in a passing
  single-process test).
- **debuggers**: `ompi_debuggers.c`, `ompi_common_dll.c`, and
  `ompi_msgq_dll.c` are the TotalView message-queue DLL, driven by
  debugger-supplied memory-read callbacks (`mqs_*`).  They cannot be
  exercised without a debugger (or a mock of the callback interface).  The
  existing `predefined_gap/pad` and `dlopen` checks validate object
  padding and DSO loadability, not this code.

## Bug found while writing these tests

`MPI_Type_get_contents` (and the large-count `MPI_Type_get_contents_c`)
iterated over the caller-provided `max_datatypes` capacity rather than the
actual number of constituent datatypes, dereferencing uninitialized
handles and segfaulting on standard-conforming code that passes oversized
output arrays.  Fixed in `ompi/mpi/c/type_get_contents.c` and
`type_get_contents_c.c` (separate commit from the tests).

## Adding a test

1. Write `ompi/test/general/<module>.c` (include `"support.h"`; pick partial or
   full init).  Avoid Automake's reserved target names (`info`, `dvi`,
   `ps`, `pdf`, `html`, ...) for the program name -- e.g. the `ompi/info`
   test program is `info_t`.
2. Add it to `check_PROGRAMS` and the per-program `_SOURCES`/`_LDADD`/
   `_DEPENDENCIES` (use `$(ompi_test_ldadd)`) in `Makefile.am`.
3. Editing only `Makefile.am` does **not** require `autogen.pl`; a plain
   `make` regenerates `Makefile.in`.  (Adding a brand-new registered
   Makefile, or editing `config/*.m4`, does require `./autogen.pl` +
   reconfigure.)
