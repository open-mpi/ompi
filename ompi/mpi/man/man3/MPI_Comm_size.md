# Name

`MPI_Comm_size` - Returns the size of the group associated with a
communicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_size(MPI_Comm comm, int *size)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_SIZE(COMM, SIZE, IERROR)
    INTEGER    COMM, SIZE, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_size(comm, size, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, INTENT(OUT) :: size
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator (handle).

# Output Parameters

* `size` : Number of processes in the group of comm (integer).
* `IERROR` : Fortran only: Error status (integer).

# Description

This function indicates the number of processes involved in a
communicator. For `MPI_COMM_WORLD`, it indicates the total number of
processes available. This function is equivalent to accessing the
communicator's group with `MPI_Comm_group`, computing the `size` using
`MPI_Group_size`, and then freeing the temporary group via `MPI_Group_free.`
If the communicator is an inter-communicator (enables communication
between two groups), this function returns the `size` of the local group.
To return the `size` of the remote group, use the `MPI_Comm_remote_size`
function.

This call is often used with `MPI_Comm_rank` to determine the amount of
concurrency available for a specific library or program. `MPI_Comm_rank`
indicates the rank of the process that calls it in the range from 0 . .
. `size`-1, where `size` is the return value of `MPI_Comm_size`.

# Note

`MPI_COMM_NULL` is not considered a valid argument to this function.

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

[`MPI_Comm_group(3)`](./?file=MPI_Comm_group.md)
[`MPI_Comm_rank(3)`](./?file=MPI_Comm_rank.md)
[`MPI_Comm_compare(3)`](./?file=MPI_Comm_compare.md)
