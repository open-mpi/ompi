# Name

`MPI_Cart_get` - Retrieves Cartesian topology information associated
with a communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[],
    int coords[])
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_CART_GET(COMM, MAXDIMS, DIMS, PERIODS, COORDS, IERROR)
    INTEGER	COMM, MAXDIMS, DIMS(*), COORDS(*), IERROR
    LOGICAL	PERIODS(*)
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Cart_get(comm, maxdims, dims, periods, coords, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: maxdims
    INTEGER, INTENT(OUT) :: dims(maxdims), coords(maxdims)
    LOGICAL, INTENT(OUT) :: periods(maxdims)
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```

# Input Parameters

* `comm` : Communicator with Cartesian structure (handle).
* `maxdims` : Length of vectors dims, periods, and coords in the calling program
(integer).

# Output Parameters

* `dims` : Number of processes for each Cartesian dimension (array of
integers).
* `periods` : Periodicity (true/false) for each Cartesian dimension (array of
logicals).
* `coords` : Coordinates of calling process in Cartesian structure (array of
integers).
* `IERROR` : Fortran only: Error status (integer).

# Description

The functions `MPI_Cartdim_get` and `MPI_Cart_get` return the Cartesian
topology information that was associated with a `comm`unicator by
`MPI_Cart_create.`

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

[`MPI_Cartdim_get`(3)](MPI_Cartdim_get.html)
[`MPI_Cart_create`(3)](MPI_Cart_create.html)
