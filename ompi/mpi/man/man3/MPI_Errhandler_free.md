# Name

MPI_Errhandler_free  - Frees an MPI-style error handler.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Errhandler_free(MPI_Errhandler *errhandler)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_ERRHANDLER_FREE(ERRHANDLER, IERROR)
    INTEGER    ERRHANDLER, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Errhandler_free(errhandler, ierror)
    TYPE(MPI_Errhandler), INTENT(INOUT) :: errhandler
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `errhandler` : MPI error handler (handle). Set to MPI_ERRHANDLER_NULL on exit.

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

Marks the error handler associated with `errhandler` for deallocation and
sets `errhandler` to `MPI_ERRHANDLER_NULL`. The error handler will be
deallocated after all communicators associated with it have been
deallocated.

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
