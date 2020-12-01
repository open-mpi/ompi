# Name

`MPI_File_get_group` - Returns a duplicate of the process group of a
file.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_get_group(MPI_File fh, MPI_Group *group)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_GET_GROUP(FH, GROUP, IERROR)
    INTEGER    FH, GROUP, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_get_group(fh, group, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    TYPE(MPI_Group), INTENT(OUT) :: group
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `fh` : File handle (handle).

# Output Parameters

* `group` : Group that opened the file (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_get_group` returns a duplicate of the `group` of the communicator
used to open the file associated with `fh`. The `group` is returned in
`group`. The user is responsible for freeing `group`, using
`MPI_Group_free.`

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
