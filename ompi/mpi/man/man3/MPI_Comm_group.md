# Name

`MPI_Comm_group` - Returns the group associated with a communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_group(MPI_Comm comm, MPI_Group *group)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_GROUP(COMM, GROUP, IERROR)
      INTEGER    COMM, GROUP, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_group(comm, group, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Group), INTENT(OUT) :: group
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator.

# Output Parameters

* `group` : Group in communicator (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

If the communicator is an intercommunicator (enables communication
between two groups of processes), this function returns the local group.
To return the remote group, use the `MPI_Comm_remote_group` function.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
