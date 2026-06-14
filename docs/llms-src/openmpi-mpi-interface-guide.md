# Open MPI interface selection guide

This guide helps tools and developers choose the correct MPI language
interface when reading or generating Open MPI application code. The MPI
Standard (https://www.mpi-forum.org/docs/) is authoritative for portable MPI
semantics; this guide describes how the interfaces map onto Open MPI.

## The four MPI language interfaces

Open MPI provides the MPI APIs through four interfaces:

| Interface | Access | Use for |
|-----------|--------|---------|
| C | `#include <mpi.h>` | C and C++ application code |
| `mpif.h` | `INCLUDE 'mpif.h'` | legacy Fortran code (fixed/free form) |
| `use mpi` | `USE mpi` | legacy Fortran code wanting a module |
| `use mpi_f08` | `USE mpi_f08` | new Fortran code |

## Choosing an interface

- **C** — Choose the C interface for C or C++ code. Include `<mpi.h>`. MPI
  handles are opaque C types (`MPI_Comm`, `MPI_Datatype`, `MPI_Request`, …)
  and almost every routine returns an `int` error code.

- **`use mpi_f08`** — **Prefer this for new Fortran code.** It is the modern,
  type-safe Fortran 2008 interface: handles are derived types (for example
  `TYPE(MPI_Comm)`, `TYPE(MPI_Datatype)`), procedures have explicit
  interfaces so the compiler checks argument types, and the final `ierror`
  argument is `OPTIONAL`.

- **`mpif.h` and `use mpi`** — These are **legacy** interfaces, retained for
  compatibility with older Fortran code. In both, MPI handles are default
  `INTEGER`s and `ierror` is a required final argument. They expose the same
  procedure signatures and differ only in how they are accessed
  (`INCLUDE 'mpif.h'` versus `USE mpi`); the `use mpi` module additionally
  provides some compile-time checking that `mpif.h` does not. Use these only
  when extending existing code that already uses them.

## Wrapper compilers

Compile and link MPI applications with Open MPI's **wrapper compilers**, which
add the correct include and library flags automatically. Do not invoke the
underlying compiler with hand-written MPI flags.

| Language | Wrapper |
|----------|---------|
| C | `mpicc` |
| C++ | `mpicxx` (also `mpic++`) |
| Fortran (all interfaces) | `mpifort` (also legacy `mpif77`/`mpif90`) |

For example:

```sh
mpicc   -o hello hello.c
mpifort -o hello hello.f90
```

## Large-count (`_c`) variants

MPI-4.0 added large-count variants of routines that take element counts, so
counts can exceed the range of a C `int`. In the C interface these have a
`_c` suffix and take `MPI_Count`/`MPI_Aint` arguments instead of `int`:

```c
int MPI_Send  (const void *buf, int       count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
int MPI_Send_c(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
```

Use the `_c` variant when a count (or displacement) may exceed `INT_MAX`. In
the `use mpi_f08` interface the same routine is overloaded for large counts
via `INTEGER(KIND=MPI_COUNT_KIND)` arguments (no separate name). In the
catalog (`openmpi-mpi-api.jsonl`), large-count bindings are marked with
`"large_count": true`.

## Open MPI extensions (`MPIX_*` and `OMPI_*`)

Symbols prefixed with `MPIX_` or `OMPI_` are **Open MPI extensions**, not part
of the MPI Standard, and are **not portable** to other MPI implementations.
Examples include the ULFM fault-tolerance routines (`MPIX_Comm_*`) and
`MPIX_Query_cuda_support`. Guard their use with the preprocessor (for example,
check `OMPI_HAVE_MPI_EXT` after including `<mpi-ext.h>`) so the code still
compiles with other MPI implementations. In the catalog these records have
`"kind": "extension"`.
