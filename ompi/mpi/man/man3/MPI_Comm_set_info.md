# Name

`MPI_Comm_set_info` - Set communicator info hints

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Comm_set_info(MPI_Comm comm, MPI_Info info)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_SET_INFO(COMM, INFO, IERROR)
    INTEGER    COMM, INFO, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Comm_set_info(comm, info, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Info), INTENT(IN) :: info
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* `comm` : Communicator on which to set info hints
* `info` : Info object containing hints to be set on *comm*

# Output Parameters

* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_COMM_SET_INFO` sets new values for the hints of the communicator
associated with comm. `MPI_COMM_SET_INFO` is a collective routine. The
`info` object may be different on each process, but any `info` entries that
an implementation requires to be the same on all processes must appear
with the same value in each process's `info` object.

The following `info` key assertions may be accepted by Open MPI:

* `mpi_assert_no_any_tag` (boolean): If set to true, then the
  implementation may assume that the process will not use the `MPI_ANY_TAG`
  wildcard on the given communicator.

* `mpi_assert_no_any_source` (boolean): If set to true, then the
  implementation may assume that the process will not use the
  `MPI_ANY_SOURCE` wildcard on the given communicator.

* `mpi_assert_exact_length` (boolean): If set to true, then the
  implementation may assume that the lengths of messages received by the
  process are equal to the lengths of the corresponding receive buffers,
  for point-to-point communication operations on the given communicator.

* `mpi_assert_allow_overtaking` (boolean): If set to true, then the
  implementation may assume that point-to-point communications on the
  given communicator do not rely on the non-overtaking rule specified in
  MPI-3.1 Section 3.5. In other words, the application asserts that send
  operations are not required to be matched at the receiver in the order
  in which the send operations were performed by the sender, and receive
  operations are not required to be matched in the order in which they
  were performed by the receiver.

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

[`MPI_Info_create(3)`](./?file=MPI_Info_create.md)
[`MPI_Info_set(3)`](./?file=MPI_Info_set.md)
[`MPI_Info_free(3)`](./?file=MPI_Info_free.md)
[`MPI_Comm_get_info(3)`](./?file=MPI_Comm_get_info.md)
