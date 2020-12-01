# Name

MPI_Errhandler_create  - Creates an MPI-style error handler -- use
of this routine is deprecated.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Errhandler_create(MPI_Handler_function *function,

    MPI_Errhandler *errhandler)
```

## Fortran Syntax

```fortran
INCLUDE 'mpif.h'

MPI_ERRHANDLER_CREATE(FUNCTION, ERRHANDLER, IERROR)
    EXTERNAL    FUNCTION
    INTEGER    ERRHANDLER, IERROR
```


# Input Parameter

* `function` : User-defined error handling procedure.

# Output Parameters

* `errhandler` : MPI error handler (handle).
* `IERROR` : Fortran only: Error status (integer).

# Description

Note that use of this routine is `deprecated` as of MPI-2. Please use
`MPI_Comm_create_errhandler` instead.
Registers the user routine `function` for use as an MPI error handler.
Returns in `errhandler` a handle to the registered error handler.
In the C language, the user routine should be a C `function` of type
`MPI_Handler_function`, which is defined as
        typedef void (MPI_Handler_function)(MPI_Comm , int , ...);
The first argument is the communicator in use. The second is the error
code to be returned by the MPI routine that raised the error. If the
routine would have returned `MPI_ERR_IN_STATUS`, it is the error code
returned in the status for the request that caused the error handler to
be invoked. The remaining arguments are stdargs arguments whose number
and meaning is implementation-dependent. An implementation should
clearly document these arguments. Addresses are used so that the handler
may be written in Fortran.

# Note

The MPI-1 Standard states that an implementation may make the output
value (errhandler) simply the address of the function. However, the
action of `MPI_Errhandler`_ free makes this impossible, since it is
required to set the value of the argument to `MPI_ERRHANDLER_NULL`. In
addition, the actual error handler must remain until all communicators
that use it are freed.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the `function` and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O `function` errors. The error handler may be changed with
`MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN`
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# See Also

[`MPI_Comm_create_errhandler(3)`](./?file=MPI_Comm_create_errhandler.md)
[`MPI_Comm_create_errhandler(3)`](./?file=MPI_Comm_create_errhandler.md)
[`MPI_Comm_get_errhandler(3)`](./?file=MPI_Comm_get_errhandler.md)
[`MPI_Comm_set_errhandler(3)`](./?file=MPI_Comm_set_errhandler.md)
