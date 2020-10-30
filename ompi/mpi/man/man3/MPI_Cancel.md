# Name

`MPI_Cancel` - Cancels a communication request.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Cancel(MPI_Request *request)
```

## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_CANCEL(REQUEST, IERROR)
    INTEGER	REQUEST, IERROR
```

## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Cancel(request, ierror)
    TYPE(MPI_Request), INTENT(IN) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameter

* `request` : Communication request (handle).

# Output Parameter

* `IERROR` : Fortran only: Error status (integer).

# Description

The `MPI_Cancel` operation allows pending communications to be canceled.
This is required for cleanup. Posting a send or a receive ties up user
resources (send or receive buffers), and a cancel may be needed to free
these resources gracefully.

A call to `MPI_Cancel` marks for cancellation a pending, nonblocking
communication operation (send or receive). The cancel call is local. It
returns immediately, possibly before the communication is actually
canceled. It is still necessary to complete a communication that has
been marked for cancellation, using a call to `MPI_Request_free,`
`MPI_Wait`, or `MPI_Test` (or any of the derived operations).

If a communication is marked for cancellation, then an `MPI_Wait` call for
that communication is guaranteed to return, irrespective of the
activities of other processes (i.e., `MPI_Wait` behaves as a local
function); similarly if `MPI_Test` is repeatedly called in a busy wait
loop for a canceled communication, then `MPI_Test` will eventually be
successful.

`MPI_Cancel` can be used to cancel a communication that uses a persistent
`request` (see Section 3.9 in the MPI-1 Standard, "Persistent
Communication Requests") in the same way it is used for nonpersistent
`request`s. A successful cancellation cancels the active communication,
but not the `request` itself. After the call to `MPI_Cancel` and the
subsequent call to `MPI_Wait` or `MPI_Test`, the `request` becomes inactive
and can be activated for a new communication.

The successful cancellation of a buffered send frees the buffer space
occupied by the pending message.

Either the cancellation succeeds or the communication succeeds, but not
both. If a send is marked for cancellation, then it must be the case
that either the send completes normally, in which case the message sent
is received at the destination process, or that the send is successfully
canceled, in which case no part of the message is received at the
destination. Then, any matching receive has to be satisfied by another
send. If a receive is marked for cancellation, then it must be the case
that either the receive completes normally, or that the receive is
successfully canceled, in which case no part of the receive buffer is
altered. Then, any matching send has to be satisfied by another receive.

If the operation has been canceled, then information to that effect will
be returned in the status argument of the operation that completes the
communication.

# Notes

The primary expected use of `MPI_Cancel` is in multi-buffering schemes,
where speculative `MPI_Irecvs` are made. When the computation completes,
some of these `request`s may remain; using `MPI_Cancel` allows the user to
cancel these unsatisfied `request`s.

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

[`MPI_Probe`(3)](MPI_Probe.html)
[`MPI_Iprobe`(3)](MPI_Iprobe.html)
[`MPI_Test_cancelled`(3)](MPI_Test_cancelled.html)
[`MPI_Cart_coords`(3)](MPI_Cart_coords.html)
