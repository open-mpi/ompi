# Name

`MPI_File_get_position_shared` - Returns the current position of the
shared file pointer.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_get_position_shared(MPI_File fh, MPI_Offset *offset)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_GET_POSITION_SHARED(FH, OFFSET, IERROR)
    INTEGER    FH, IERROR

    INTEGER(KIND=MPI_OFFSET_KIND)    OFFSET
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_get_position_shared(fh, offset, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: offset
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `fh` : File handle (handle).

# Output Parameters

* `offset` : Offset of the shared file pointer (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_get_position_shared` returns, in `offset`, the current position
of the shared file pointer in `etype` units relative to the current
displacement and file type.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the OFFSET
argument only for Fortran 90. Sun FORTRAN 77 users may use the
non-portable syntax
where `MPI_ADDRESS_KIND` is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
`MPI_ERRORS_RETURN`. The error handler may be changed with
`MPI_File_set_errhandler`; the predefined error handler
`MPI_ERRORS_ARE_FATAL` may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
