# Name

`MPI_Session_get_info` - Returns an info object containing the hints of an MPI Session

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Session_get_info(MPI_Session session, MPI_Info *info_used)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_SESSION_GET_INFO(SESSION, INFO_USED)
    INTEGER	SESSION, INFO_USED
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Session_get_info(session, info_used)
    TYPE(MPI_Session), INTENT(IN) :: session
    TYPE(MPI_Info), INTENT(OUT) :: info_used
```

# Input Parameters

* `session` : session (handle)

# Output Parameters

* `info_used`: info object (handle)
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Session_get_info` returns a new info object containing the hints of the MPI
Session associated with session. The current setting of all hints related to this MPI Session
is returned in `info_used`. An MPI implementation is required to return all hints that are
supported by the implementation and have default values specified; any user-supplied hints
that were not ignored by the implementation; and any additional hints that were set by
the implementation. If no such hints exist, a handle to a newly created info object is
returned that contains no key/value pair.

# Notes

The user is responsible for freeing info_used via ` MPI_Info_free`.

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
