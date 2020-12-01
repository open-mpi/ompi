# Name

MPI_Errhandler_set  - Sets the error handler for a communicator --
use of this routine is deprecated.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Errhandler_set(MPI_Comm comm, MPI_Errhandler errhandler)
```

## Fortran Syntax

```fortran
INCLUDE 'mpif.h'

MPI_ERRHANDLER_SET(COMM, ERRHANDLER, IERROR)
    INTEGER    COMM, ERRHANDLER, IERROR
```


# Input Parameters

* `comm` : Communicator to set the error handler for (handle).
* `errhandler` : New MPI error handler for communicator (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

Note that use of this routine is `deprecated` as of MPI-2. Please use
`MPI_Comm_set_errhandler` instead.
Associates the new error handler `errhandler` with `comm`unicator `comm` at
the calling process. Note that an error handler is always associated
with the communicator.

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
