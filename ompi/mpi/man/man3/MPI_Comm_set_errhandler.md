# Name

`MPI_Comm_set_errhandler` - Attaches a new error handler to a
communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_set_errhandler(MPI_Comm comm,
    MPI_Errhandler errhandler)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_SET_ERRHANDLER(COMM, ERRHANDLER, IERROR)
    INTEGER    COMM, ERRHANDLER, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_set_errhandler(comm, errhandler, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `comm` : Communicator (handle).

# Output Parameters

* `errhandler` : New error handler for communicator (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Comm_set_errhandler` attaches a new error handler to a communicator.
The error handler must be either a predefined error handler or an error
handler created by a call to `MPI_Comm_create_errhandler`. This call is
identical to `MPI_Errhandler_set`, the use of which is deprecated.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
