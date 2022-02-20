# Name

MPI_Get_library_version - Returns a string of the current Open MPI
version

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Get_library_version(char *version, int *resultlen)
```


## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GET_LIBRARY_VERSION(VERSION, RESULTLEN, IERROR)
    CHARACTER*(*)	NAME
    INTEGER RESULTLEN, IERROR
```


## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Get_library_version(version, resulten, ierror)
    CHARACTER(LEN=MPI_MAX_LIBRARY_VERSION_STRING), INTENT(OUT) :: version
    INTEGER, INTENT(OUT) :: resultlen
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Output Parameters

* version : A string containing the Open MPI version (string).
* resultlen : Length (in characters) of result returned in version (integer).
* IERROR : Fortran only: Error status (integer).

# Description

This routine returns a string representing the version of the MPI
library. The version argument is a character string for maximum
flexibility.

The number of characters actually written is returned in the output
argument, resultlen. In C, a '0' character is additionally stored
at version[resultlen]. The resultlen cannot be larger than
(MPI_MAX_LIBRARY_VERSION_STRING - 1). In Fortran, version is padded on
the right with blank characters. The resultlen cannot be larger than MPI_MAX_LIBRARY_VERSION_STRING.

# Note

The version string that is passed must be at least
MPI_MAX_LIBRARY_VERSION_STRING characters long.

MPI_Get_library_version is one of the few functions that can be called
before MPI_Init and after MPI_Finalize.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
MPI_Comm_set_errhandler; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[MPI_Get_version(3)](MPI_Get_version.html)
