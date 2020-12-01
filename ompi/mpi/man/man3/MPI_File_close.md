# Name

`MPI_File_close` - Closes a file (collective).

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_close(MPI_File *fh)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_CLOSE(FH, IERROR)
    INTEGER    FH, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_close(fh, ierror)
    TYPE(MPI_File), INTENT(INOUT) :: fh
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input/Output Parameter

* `fh` : File handle (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_close` first synchronizes file state, then closes the file
associated with `fh`. `MPI_File_close` is a collective routine. The user
is responsible for ensuring that all outstanding requests associated
with `fh` have completed before calling `MPI_File_close.`

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
