# Open MPI examples

A small, representative set of MPI examples for Open MPI. The goal is coverage
of common patterns, not exhaustive API coverage. The MPI Standard
(https://www.mpi-forum.org/docs/) is authoritative for portable semantics.

Compile and run with Open MPI's wrapper compilers:

```sh
mpicc   -o example example.c        # C
mpifort -o example example.f90      # Fortran
mpirun -n 4 ./example
```

The "hello world" and "ring" programs below are the canonical Open MPI example
programs that ship in the source tree under `examples/` (and are compiled in
CI). The remaining C examples illustrate additional common patterns.

## C: initialize and finalize (`examples/hello_c.c`)

```c
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int rank, size, len;
    char version[MPI_MAX_LIBRARY_VERSION_STRING];

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Get_library_version(version, &len);
    printf("Hello, world, I am %d of %d, (%s, %d)\n", rank, size, version, len);
    MPI_Finalize();

    return 0;
}
```

## C: point-to-point send/receive (`examples/ring_c.c`)

```c
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int rank, size, next, prev, message, tag = 201;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    next = (rank + 1) % size;
    prev = (rank + size - 1) % size;

    if (0 == rank) {
        message = 10;
        MPI_Send(&message, 1, MPI_INT, next, tag, MPI_COMM_WORLD);
    }

    while (1) {
        MPI_Recv(&message, 1, MPI_INT, prev, tag, MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);
        if (0 == rank) {
            --message;
        }
        MPI_Send(&message, 1, MPI_INT, next, tag, MPI_COMM_WORLD);
        if (0 == message) {
            break;
        }
    }

    if (0 == rank) {
        MPI_Recv(&message, 1, MPI_INT, prev, tag, MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);
    }

    MPI_Finalize();
    return 0;
}
```

## C: collective communication (allreduce)

```c
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int rank, size, local, total;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    local = rank + 1;
    MPI_Allreduce(&local, &total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (0 == rank) {
        printf("Sum of ranks+1 across %d processes = %d\n", size, total);
    }

    MPI_Finalize();
    return 0;
}
```

## C: nonblocking communication

```c
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int rank, size, send_val, recv_val, next, prev, tag = 0;
    MPI_Request reqs[2];

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    next = (rank + 1) % size;
    prev = (rank + size - 1) % size;
    send_val = rank;

    MPI_Irecv(&recv_val, 1, MPI_INT, prev, tag, MPI_COMM_WORLD, &reqs[0]);
    MPI_Isend(&send_val, 1, MPI_INT, next, tag, MPI_COMM_WORLD, &reqs[1]);
    MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);

    printf("Rank %d received %d from rank %d\n", rank, recv_val, prev);

    MPI_Finalize();
    return 0;
}
```

## C: derived datatype (contiguous)

```c
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int rank, tag = 0;
    double buf[4] = {0, 0, 0, 0};
    MPI_Datatype vec4;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Type_contiguous(4, MPI_DOUBLE, &vec4);
    MPI_Type_commit(&vec4);

    if (0 == rank) {
        for (int i = 0; i < 4; i++) buf[i] = i * 1.5;
        MPI_Send(buf, 1, vec4, 1, tag, MPI_COMM_WORLD);
    } else if (1 == rank) {
        MPI_Recv(buf, 1, vec4, 0, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        printf("Rank 1 received: %g %g %g %g\n", buf[0], buf[1], buf[2], buf[3]);
    }

    MPI_Type_free(&vec4);
    MPI_Finalize();
    return 0;
}
```

## C: communicators (split)

```c
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int rank, color, subrank;
    MPI_Comm subcomm;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    color = rank % 2;  /* split into even/odd groups */
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, &subcomm);
    MPI_Comm_rank(subcomm, &subrank);

    printf("World rank %d -> color %d, subcomm rank %d\n", rank, color, subrank);

    MPI_Comm_free(&subcomm);
    MPI_Finalize();
    return 0;
}
```

## C: MPI-IO (collective write)

```c
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int rank;
    MPI_File fh;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_File_open(MPI_COMM_WORLD, "out.dat",
                  MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
    MPI_File_set_view(fh, rank * (MPI_Offset) sizeof(int),
                      MPI_INT, MPI_INT, "native", MPI_INFO_NULL);
    MPI_File_write_all(fh, &rank, 1, MPI_INT, MPI_STATUS_IGNORE);
    MPI_File_close(&fh);

    MPI_Finalize();
    return 0;
}
```

## C: one-sided / RMA (window + put)

```c
#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int rank, size, value, *winbuf;
    MPI_Win win;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Alloc_mem(sizeof(int), MPI_INFO_NULL, &winbuf);
    *winbuf = 0;
    MPI_Win_create(winbuf, sizeof(int), sizeof(int), MPI_INFO_NULL,
                   MPI_COMM_WORLD, &win);

    value = rank;
    MPI_Win_fence(0, win);
    /* Each rank writes its value into rank 0's window. */
    MPI_Put(&value, 1, MPI_INT, 0, 0, 1, MPI_INT, win);
    MPI_Win_fence(0, win);

    if (0 == rank) {
        printf("Rank 0 window holds %d after RMA\n", *winbuf);
    }

    MPI_Win_free(&win);
    MPI_Free_mem(winbuf);
    MPI_Finalize();
    return 0;
}
```

## Fortran `use mpi_f08`: initialize and finalize (`examples/hello_usempif08.f90`)

```fortran
program main
    use mpi_f08
    implicit none
    integer :: rank, size, len
    character(len=MPI_MAX_LIBRARY_VERSION_STRING) :: version

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size)
    call MPI_GET_LIBRARY_VERSION(version, len)
    write(*, '("Hello, world, I am ", i2, " of ", i2, ": ", a)') &
          rank, size, version
    call MPI_FINALIZE()
end
```

## Fortran `use mpi`: initialize and finalize (`examples/hello_usempi.f90`)

```fortran
program main
    use mpi
    implicit none
    integer :: rank, size, ierror

    call MPI_INIT(ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    write(*, '("Hello, world, I am ", i2, " of ", i2)') rank, size
    call MPI_FINALIZE(ierror)
end
```

## Fortran `mpif.h`: initialize and finalize (`examples/hello_mpifh.f`)

```fortran
      program main
      implicit none
      include 'mpif.h'
      integer rank, size, ierror

      call MPI_INIT(ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
      print *, 'Hello, world, I am ', rank, ' of ', size
      call MPI_FINALIZE(ierror)
      end
```

## Compiling and running

```sh
mpicc   -o hello   hello_c.c        && mpirun -n 4 ./hello
mpicc   -o ring    ring_c.c         && mpirun -n 4 ./ring
mpifort -o hellof  hello_usempif08.f90 && mpirun -n 4 ./hellof
```

The canonical `hello_*` and `ring_*` programs (C, `mpif.h`, `use mpi`, and
`use mpi_f08`) live in the Open MPI source tree under `examples/` and are
compiled in CI via `examples/Makefile`. The additional C examples above are
documentation examples.
