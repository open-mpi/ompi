# Name

MPI_Cart_map - Maps process to Cartesian topology information.

# Syntax

## C Syntax

```C
#include <mpi.h>

int MPI_Cart_map(MPI_Comm comm, int ndims, const int dims[],
    const int periods[], int *newrank)
```


## Fortran Syntax

```Fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_CART_MAP(COMM, NDIMS, DIMS, PERIODS, NEWRANK, IERROR)
    INTEGER	COMM, NDIMS, DIMS(*), NEWRANK, IERROR
    LOGICAL	PERIODS(*)
```


## Fortran 2008 Syntax

```Fortran
USE mpi_f08

MPI_Cart_map(comm, ndims, dims, periods, newrank, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: ndims, dims(ndims)
    LOGICAL, INTENT(IN) :: periods(ndims)
    INTEGER, INTENT(OUT) :: newrank
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* comm : Input communicator (handle).
* ndims : Number of dimensions of Cartesian structure (integer).
* dims : Integer array of size ndims specifying the number of processes in
each coordinate direction.
* periods : Logical array of size ndims specifying the periodicity specification
in each coordinate direction.

# Output Parameters

* newrank : Reordered rank of the calling process; MPI_UNDEFINED if calling
process does not belong to grid (integer).
* IERROR : Fortran only: Error status (integer).

# Description

MPI_Cart_map and MPI_Graph_map can be used to implement all other
topology functions. In general they will not be called by the user
directly, unless he or she is creating additional virtual topology
capability other than that provided by MPI.
MPI_Cart_map computes an "optimal" placement for the calling process
on the physical machine. A possible implementation of this function is
to always return the rank of the calling process, that is, not to
perform any reordering.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
MPI_Comm_set_errhandler; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[MPI_Graph_map(3)](MPI_Graph_map.html)
