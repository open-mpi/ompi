# Name

`MPI_Session_init` - Creates a new session handle

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Session_init(MPI_Info info, MPI_Errhandler errhandler, MPI_Session *session)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_SESSION_INIT(INFO, ERRHANDLER, SESSION, IERROR)
    INTEGER	INFO, ERRHANDLER, SESSION, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Session_init(info, errhandler, session, ierror)
    TYPE(MPI_Info), INTENT(IN) :: info
    TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
    TYPE(MPI_Session), INTENT(OUT) :: session
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```

# Input Parameters

* `info` : info object (handle)
* `errhandler` : error handler to be attached to the returned session (handle)

# Output Parameters

* `session` : New session (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

`MPI_Session_init` is used to instantiate an MPI Session.  The returned session handle
can be used to query the runtime system about characteristics of the job within which the process is running, as well as other system resources.
An application can make multiple calls to `MPI_Session_init` and the related `MPI_Session_finalize` routine.

# Notes

The info argument is used to request MPI functionality requirements and possible MPI
implementation specific capabilities.

The `errhandler` argument specifies an error handler to invoke in the event that the
Session instantiation call encounters an error.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors.  The predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Session_get_num_psets`(3)](MPI_Session_get_num_psets.html)
[`MPI_Session_get_nth_pset`(3)](MPI_Session_get_nth_pset.html)
[`MPI_Session_group_from_pset`(3)](MPI_Session_group_from_pset.html)
[`MPI_Session_finalize`(3)](MPI_Session_finalize.html)
