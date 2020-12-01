# Name

`MPI_File_create_errhandler` - Creates an MPI-style error handler
that can be attached to a file.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_create_errhandler(MPI_File_errhandler_function *function,

    MPI_Errhandler *errhandler)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_CREATE_ERRHANDLER(FUNCTION, ERRHANDLER, IERROR)
    EXTERNAL    FUNCTION
    INTEGER    ERRHANDLER, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_create_errhandler(file_errhandler_fn, errhandler, ierror)
    PROCEDURE(MPI_File_errhandler_function) :: file_errhandler_fn
    TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Deprecated Type Name Note

MPI-2.2 deprecated the `MPI_File_errhandler_fn` type in favor of
`MPI_File_errhandler_function`. Open MPI supports both
names (indeed, the `_fn` names are typedefs to the `_function` names).

# Input Parameter

* `function` : User-defined error handling procedure (function).

# Output Parameters

* `errhandler` : MPI error handler (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

Registers the user routine `function` for use as an MPI error handler.
Returns in `errhandler` a handle to the registered error handler.
In the C language, the user routine ``function`` should be a C `function` of
type `MPI_File_errhandler_function`, which is defined as
        typedef void (MPI_File_errhandler_function)(MPI_File , int ,
The first argument to `function` is the file in use. The second is the
error code to be returned by the MPI routine that raised the error.
In the Fortran language, the user routine should be of the form:

# Errors

Almost all MPI routines return an error value; C routines as the value
of the `function` and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O `function` errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
