# Name

`MPI_Comm_remote_size` - Determines the size of the remote group
associated with an intercommunicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_remote_size(MPI_Comm comm, int *size)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_REMOTE_SIZE(COMM, SIZE, IERROR)
    INTEGER    COMM, SIZE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_remote_size(comm, size, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(OUT) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator (handle).

# Output Parameters

* `size` : Number of processes in the remote group of comm (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Comm_remote_size` determines the `size` of the remote group associated
with an intercommunicator.
The intercommunicator accessors (`MPI_Comm_test_inter`,
`MPI_Comm_remote_size`, `MPI_Comm_remote_group`) are all local operations.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Comm_test_inter(3)`](./?file=MPI_Comm_test_inter.md)
[`MPI_Comm_remote_group(3)`](./?file=MPI_Comm_remote_group.md)
[`MPI_Intercomm_create(3)`](./?file=MPI_Intercomm_create.md)
[`MPI_Intercomm_merge(3)`](./?file=MPI_Intercomm_merge.md)
