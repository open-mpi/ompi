# Name

`MPI_File_get_view` - Returns the process's view of data in the file.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_File_get_view(MPI_File fh, MPI_Offset *disp,

    MPI_Datatype *etype, MPI_Datatype *filetype,
    char *datarep)
```

## Fortran Syntax (See Fortran 77 Notes)

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_FILE_GET_VIEW(FH, DISP, ETYPE,
    FILETYPE, DATAREP, IERROR)
    INTEGER    FH, ETYPE, FILETYPE, IERROR
    CHARACTER*(*)    DATAREP

    INTEGER(KIND=MPI_OFFSET_KIND)    DISP
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_File_get_view(fh, disp, etype, filetype, datarep, ierror)
    TYPE(MPI_File), INTENT(IN) :: fh
    INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: disp
    TYPE(MPI_Datatype), INTENT(OUT) :: etype, filetype
    CHARACTER(LEN=*), INTENT(OUT) :: datarep
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `fh` : File handle (handle).

# Output Parameters

* `disp` : Displacement (integer).
* `etype` : Elementary data type (handle).
* `filetype` : File type (handle). See Restrictions, below.
* `datarep` : Data representation (string).
* `IERROR` : Fortran only: Error status (integer).

# Description

The `MPI_File_get_view` routine returns the process's view of the data in
the file. The current values of the displacement, etype, and filetype
are returned in `disp`, `etype`, and `filetype`, respectively.
The `MPI_File_get_view` interface allows the user to pass a
data-representation string via the `datarep` argument.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the DISP
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax.
where `MPI_OFFSET_KIND` is a constant defined in mpif.h and gives the
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
