# Name

`MPI_File_get_size` - Returns the current size of the file.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_get_size(MPI_File fh, MPI_Offset *size)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_GET_SIZE(FH, SIZE, IERROR)
    INTEGER    FH, ERROR

    INTEGER(KIND=MPI_OFFSET_KIND)    SIZE
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_get_size(fh, size, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `fh` : File handle (handle).
* `size` : Size of the file in bytes (integer).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_File_get_size` returns, in `size` , the current `size` in bytes of the
file associated with the file handle fh. Note that the file size
returned by Solaris may not represent the number of bytes physically
allocated for the file in those cases where all bytes in this file have
not been written at least once.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the SIZE
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

# See Also


[`MPI_File_preallocate(3)`](./?file=MPI_File_preallocate.md)
