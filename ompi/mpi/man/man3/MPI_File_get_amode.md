# Name

`MPI_File_get_amode` - Returns access mode associated with an open
file.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_get_amode(MPI_File fh, int *amode)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_GET_AMODE(FH, AMODE, IERROR)
    INTEGER    FH, AMODE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_get_amode(fh, amode, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER, INTENT(OUT) :: amode
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `fh` : File handle (handle).

# Output Parameters

* `amode` : File access mode used to open the file (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_get_amode` returns, in `amode`, the access mode associated with
the open file fh.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
