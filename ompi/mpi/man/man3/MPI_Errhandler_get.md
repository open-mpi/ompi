# Name

MPI_Errhandler_get  - Gets the error handler for a communicator --
use of this routine is deprecated.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler)
```

## Fortran Syntax

```fortran
INCLUDE 'mpif.h'

MPI_ERRHANDLER_GET(COMM, ERRHANDLER, IERROR)
    INTEGER    COMM, ERRHANDLER, IERROR
```


# Input Parameter

* `comm` : Communicator to get the error handler from (handle).

# Output Parameters

* `errhandler` : MPI error handler currently associated with communicator (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

Note that use of this routine is `deprecated` as of MPI-2. Please use
`MPI_Comm_get_errhandler` instead.
Returns in `errhandler` (a handle to) the error handler that is currently
associated with communicator comm.
Example: A library function may register at its entry point the
current error handler for a communicator, set its own private error
handler for this communicator, and restore before exiting the previous
error handler.

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

[`MPI_Comm_create_errhandler(3)`](./?file=MPI_Comm_create_errhandler.md)
[`MPI_Comm_create_errhandler(3)`](./?file=MPI_Comm_create_errhandler.md)
[`MPI_Comm_get_errhandler(3)`](./?file=MPI_Comm_get_errhandler.md)
[`MPI_Comm_set_errhandler(3)`](./?file=MPI_Comm_set_errhandler.md)
