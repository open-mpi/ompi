# Name

`MPI_File_call_errhandler` - Passes the supplied error code to the
error handler assigned to a file

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_call_errhandler(MPI_File fh, int errorcode)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_CALL_ERRHANDLER(FH, ERRORCODE, IERROR)
    INTEGER    FH, IERRORCODE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_call_errhandler(fh, errorcode, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER, INTENT(IN) :: errorcode
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `fh` : file with error handler (handle).
* `errorcode` : MPI error code (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

This function invokes the error handler assigned to the file handle fh
with the supplied error code errorcode. If the error handler was
successfully called, the process is not aborted, and the error handler
returns, this function returns `MPI_SUCCESS.`
Unlike errors on communicators and windows, the default errorhandler for
files is `MPI_ERRORS_RETURN.`

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
See the MPI man page for a full list of MPI error codes.

# See Also

[`MPI_File_create_errhandler(3)`](./?file=MPI_File_create_errhandler.md)
[`MPI_File_set_errhandler(3)`](./?file=MPI_File_set_errhandler.md)
