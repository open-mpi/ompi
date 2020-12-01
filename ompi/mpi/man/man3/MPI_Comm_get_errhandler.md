# Name

`MPI_Comm_get_errhandler` - Retrieves error handler associated with a
communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_get_errhandler(MPI_Comm comm,
    MPI_Errhandler *errhandler)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_GET_ERRHANDLER(COMM, ERRHANDLER, IERROR)
    INTEGER    COMM, ERRHANDLER, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_get_errhandler(comm, errhandler, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator (handle).

# Output Parameters

* `errhandler` : New error handler for communicator (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Comm_get_errhandler` retrieves the error handler currently associated
with a communicator. This call is identical to `MPI_Errhandler_get`, the
use of which is deprecated.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
See the MPI man page for a full list of MPI error codes.
