# Name

`MPI_Graphdims_get` - Retrieves graph topology information associated
with a communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GRAPHDIMS_GET(COMM, NNODES, NEDGES, IERROR)
    INTEGER	COMM, NNODES, NEDGES, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Graphdims_get(comm, nnodes, nedges, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(OUT) :: nnodes, nedges
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator for group with graph structure (handle).

# Output Parameters

* `nnodes` : Number of nodes in graph (integer).
* `nedges` : Number of edges in graph (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

Functions `MPI_Graphdims_get` and `MPI_Graph_get` retrieve the
graph-topology information that was associated with a communicator by
`MPI_Graph_create`.

The information provided by `MPI_Graphdims_get` can be used to dimension
the vectors index and edges correctly for a call to `MPI_Graph_get`.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Graph_create`(3)](MPI_Graph_create.html)
[`MPI_Graph_get`(3)](MPI_Graph_get.html)
