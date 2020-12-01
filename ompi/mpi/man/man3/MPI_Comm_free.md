# Name

`MPI_Comm_free` - Mark a communicator object for deallocation.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_free(MPI_Comm *comm)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_FREE(COMM, IERROR)
    INTEGER    COMM, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_free(comm, ierror)
    TYPE(MPI_Comm), INTENT(INOUT) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `comm` : Communicator to be destroyed (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

This operation marks the communicator object for deallocation. The
handle is set to `MPI_COMM_NULL`. Any pending operations that use this
communicator will complete normally; the object is actually deallocated
only if there are no other active references to it. This call applies to
intracommunicators and intercommunicators. Upon actual deallocation, the
delete callback functions for all cached attributes (see Section 5.7 in
the MPI-1 Standard, "Caching") are called in arbitrary order.

# Notes

Note that it is not defined by the MPI standard what happens if the
`delete_fn` callback invokes other MPI functions. In Open MPI, it is not
valid for `delete_fn` callbacks (or any of their children) to add or
delete attributes on the same object on which the `delete_fn` callback is
being invoked.

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

[`MPI_Comm_delete_attr(3)`](./?file=MPI_Comm_delete_attr.md)
