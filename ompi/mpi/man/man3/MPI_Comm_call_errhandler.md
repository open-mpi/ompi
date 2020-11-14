# Name

`MPI_Comm_call_errhandler` - Passes the supplied error code to the
error handler assigned to a communicator

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_call_errhandler(MPI_Comm comm, int errorcode)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_CALL_ERRHANDLER(COMM, ERRORCODE, IERROR)
    INTEGER	COMM, ERRORCODE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_call_errhandler(comm, errorcode, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(IN) :: errorcode
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : communicator with error handler (handle).
* `errorcode` : error code (integer).

# Output Parameters

* `IERROR` : Fortran only: Error status (integer).

# Description

This function invokes the error handler assigned to the `comm`unicator
`comm` with the supplied error code `errorcode`. If the error handler
was successfully called, the process is not aborted, and the error
handler returns, this function returns `MPI_SUCCESS.`

# Notes

Users should note that the default error handler is
`MPI_ERRORS_ARE_FATAL`. Thus, calling this function will abort the
processes in `comm` if the default error handler has not been changed.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
See the MPI man page for a full list of MPI error codes.

# See Also

[`MPI_Comm_create_errhandler`(3)](./?file=MPI_Comm_create_errhandler.md)
[`MPI_Comm_set_errhandler`(3)](./?file=MPI_Comm_set_errhandler.md)
