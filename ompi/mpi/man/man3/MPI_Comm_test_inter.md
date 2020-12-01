# Name

`MPI_Comm_test_inter` - Tests to see if a comm is an
intercommunicator.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_test_inter(MPI_Comm comm, int *flag)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_TEST_INTER(COMM, FLAG, IERROR)
    INTEGER    COMM, IERROR
    LOGICAL    FLAG 
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_test_inter(comm, flag, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    LOGICAL, INTENT(OUT) :: flag
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator (handle).

# Output Parameters

* `flag` : (Logical.)
* `IERROR` : Fortran only: Error status (integer).

# Description

This local routine allows the calling process to determine the type of a
communicator. It returns true for an intercommunicator, false for an
intracommunicator.

The type of communicator also affects the value returned by three other
functions. When dealing with an intracommunicator (enables communication
within a single group), the functions listed below return the expected
values, group size, group, and rank. When dealing with an
inter-communicator, however, they return the following values:

```fortran
MPI_Comm_size    Returns the size of the local group.
MPI_Comm_group    Returns the local group.
MPI_Comm_rank    Returns the rank in the local group.
```

To return the remote group and remote group size of an
inter-communicator, use the `MPI_Comm_remote_group` and
`MPI_Comm_remote_size` functions.

The operation `MPI_Comm_compare` is valid for intercommunicators. Both
communicators must be either intra- or intercommunicators, or else
`MPI_UNEQUAL` results. Both corresponding local and remote groups must
compare correctly to get the results `MPI_CONGRUENT` and `MPI_SIMILAR`. In
particular, it is possible for `MPI_SIMILAR` to result because either the
local or remote groups were similar but not identical.

The following accessors provide consistent access to the remote group of
an intercommunicator: `MPI_Comm_remote_size`, `MPI_Comm_remote_group.`
The intercommunicator accessors (MPI_Comm_test_inter,
MPI_Comm_remote_size, MPI_Comm_remote_group) are all local operations.

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

[`MPI_Comm_remote_group(3)`](./?file=MPI_Comm_remote_group.md)
[`MPI_Comm_remote_size(3)`](./?file=MPI_Comm_remote_size.md)
[`MPI_Intercomm_create(3)`](./?file=MPI_Intercomm_create.md)
[`MPI_Intercomm_merge(3)`](./?file=MPI_Intercomm_merge.md)
