# Name

`MPI_Session_get_num_psets` - Query runtime for number of available process sets

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Session_get_num_psets(MPI_Session session, MPI_Info info, int *npset_names)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_SESSION_GET_NUM_PSETS(SESSION, INFO, NPSET_NAMES, IERROR)
    INTEGER	SESSION, INFO, SESSION, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Session_get_num_psets(session, info, npset_names, ierror)
    TYPE(MPI_Session), INTENT(IN) :: session
    TYPE(MPI_Info), INTENT(IN) :: info
    INTEGER, INTENT(OUT) :: npset_names
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```

# Input Parameters

* `session` : session (handle)
* `info`: info object (handle)

# Output Parameters

* `npset_names` : number of available process sets (non-negtive integer)
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Session_get_num_psets` is used to query the runtime for the number of available process sets in
which the calling MPI process is a member. An MPI implementation is allowed to increase
the number of available process sets during the execution of an MPI application when new
process sets become available. However, MPI implementations are not allowed to change
the index of a particular process set name, or to change the name of the process set at a
particular index, or to delete a process set name once it has been added. 

# Notes

When a process set becomes invalid, for example, when some processes become unreachable due to failures
in the communication system, subsequent usage of the process set name may raise an
error. For example, creating an `MPI_Group` from such a process set might succeed because it
is a local operation, but creating an `MPI_Comm` from that group and attempting collective
communication may raise an error.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
`MPI_Session_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Session_init`(3)](MPI_Session_init.html)
