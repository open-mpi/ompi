# Name

`MPI_Get_count`  - Gets the number of top-level elements received.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Get_count(const MPI_Status *status, MPI_Datatype datatype,
    int *count)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GET_COUNT(STATUS, DATATYPE, COUNT, IERROR)
    INTEGER	STATUS(MPI_STATUS_SIZE), DATATYPE, COUNT, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Get_count(status, datatype, count, ierror)
    TYPE(MPI_Status), INTENT(IN) :: status
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(OUT) :: count
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```

# Input Parameters

* `status` : Return status of receive operation (status).
* `datatype` : Datatype of each receive buffer element (handle).

# Output Parameters

* `count` : Number of received elements (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description


Returns the number of entries received. (We count entries, each of type
`datatype`, not bytes.) The `datatype` argument should match the argument
provided by the receive call that set the `status` variable. (As explained
in Section 3.12.5 in the MPI-1 Standard, "Use of General Datatypes in
Communication," `MPI_Get_count` may, in certain situations, return the
value `MPI_UNDEFINED`.)

The `datatype` argument is passed to `MPI_Get_count` to improve performance.
A message might be received without counting the number of elements it
contains, and the `count` value is often not needed. Also, this allows the
same function to be used after a call to `MPI_Probe`.

# Notes

If the size of the `datatype` is zero, this routine will return a `count` of
zero. If the amount of data in `status` is not an exact multiple of the
size of `datatype` (so that `count` would not be integral), a `count` of
`MPI_UNDEFINED` is returned instead.

# Errors

If the value to be returned is larger than can fit into the `count`
parameter, an `MPI_ERR_TRUNCATE` error is raised.

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Get_elements`(3)](MPI_Get_elements.html)
