# Name

`MPI_Graph_neighbors_count`  - Returns the number of neighbors of a
node associated with a graph topology.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Graph_neighbors_count(MPI_Comm comm, int rank,
    int *nneighbors)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GRAPH_NEIGHBORS_COUNT(COMM, RANK, NNEIGHBORS, IERROR)
    INTEGER	COMM, RANK, NNEIGHBORS, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Graph_neighbors_count(comm, rank, nneighbors, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: rank
    INTEGER, INTENT(OUT) :: nneighbors
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `comm` : Communicator with graph topology (handle).
* `rank` : Rank of process in group of comm (integer).

# Output Parameters

* `nneighbors` : Number of neighbors of specified process (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Graph_neighbors_count` and `MPI_Graph_neighbors` provide adjacency
information for a general, graph topology. `MPI_Graph_neighbors_count`
returns the number of neighbors for the process signified by `rank`.

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

[`MPI_Graph_neighbors`(3)](MPI_Graph_neighbors.html)
