# Name

`MPI_Group_from_session_pset` - Creates a group using a provided session handle and process set.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Group_from_session_pset(MPI_Session session, const char *pset_name, MPI_Group *newgroup)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GROUP_FROM_SESSION_PSET(SESSION, PSET_NAME, NEWGROUP, IERROR)
    INTEGER	SESSION,  NEWGROUP, IERROR
    CHARACTER*(*) PSET_NAME
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Group_from_session_pset(session, pset_name, newgroup, ierror)
    TYPE(MPI_Session), INTENT(IN) :: session
    CHARACTER(LEN=*), INTENT(IN) :: pset_name
    TYPE(MPI_Group), INTENT(OUT) :: newgroup
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```

# Input Parameters

* `session` : Session (handle).
* `pset_name` : name of process set to use to create the new group (string)

# Output Parameters

* `newgroup` : New group derived from supplied session and process set (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

The function `MPI_Group_from_session_pset` creates a group `newgroup` using the
provided `session` handle and `process set`. The process set name must be one returned from
an invocation of `MPI_Session_get_nth_pset` using the supplied `session` handle. If the
`pset_name` does not exist, MPI_GROUP_NULL will be returned in the `newgroup` argument.

# Note

As with other group constructors, `MPI_Group_from_session_pset` is a local function.

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
[`MPI_Session_get_nth_pset`(3)](MPI_Session_get_nth_pset.html)
[`MPI_Group_free`(3)](MPI_Group_free.html)
