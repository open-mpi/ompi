# Name

`MPI_Cartdim_get` - Retrieves Cartesian topology information
associated with a communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Cartdim_get(MPI_Comm comm, int *ndims)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_CARTDIM_GET(COMM, NDIMS, IERROR)
    INTEGER	COMM, NDIMS, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Cartdim_get(comm, ndims, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(OUT) :: ndims
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator with Cartesian structure (handle).

# Output Parameters

* `ndims` : Number of dimensions of the Cartesian structure (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Cartdim_get` returns the number of dimensions of the Cartesian
structure.

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

[`MPI_Cart_get`(3)](./?file=MPI_Cart_get.md)
[`MPI_Cart_create`(3)](./?file=MPI_Cart_create.md)
