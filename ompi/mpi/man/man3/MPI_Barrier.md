# NAME

MPI_Barrier, MPI_Ibarrier - Synchronization between MPI processes in a group

# Syntax

## C Syntax
```c
#include <mpi.h>
int MPI_Barrier(MPI_Comm)
int MPI_Ibarrier(MPI_Comm comm, MPI_Request *request)
```
## Fortran Syntax
```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'
MPI_BARRIER(COMM, IERROR)
    INTEGER COMM, IERROR
MPI_IBARRIER(COMM, REQUEST, IERROR)
    INTEGER COMM, REQUEST, IERROR
```
## Fortran 2008 Syntax
```fortran
USE mpi_f08
MPI_Barrier(comm, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
MPI_Ibarrier(comm, request, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Request), INTENT (OUT) :: request
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```
# Input Parameter
* `comm` : Communicator (handle).
# Output Parameters
* `request` : Request (handle, non-blocking only).
* `IERROR` : Fortran only: Error status (integer).
# Description
An MPI barrier completes after all groups members have entered the barrier.
# When Communicator is an Inter-Communicator
When the communicator is an inter-communicator, the barrier operation is performed across all processes in both groups.  All processes in the first group may exit the barrier when all processes in the second group have entered the barrier.
# Errors
Almost all MPI routines return an error value; C routines as the value of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is called. By default, this error handler aborts the MPI job, except for I/O function errors. The error handler may be changed with `MPI_Comm_set_errhandler`; the predefined error handler `MPI_ERRORS_RETURN` may be used to cause error values to be returned. Note that MPI does not guarantee that an MPI program can continue past an error.
# See Also
[`MPI_Bcast`(3)](MPI_Bcast.html)
