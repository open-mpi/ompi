# Name

MPI_Get_elements, MPI_Get_elements_x - Returns the number of basic
elements in a data type.

# Syntax

## C Syntax

```c
#include <mpi.h>

int MPI_Get_elements(const MPI_Status *status, MPI_Datatype datatype,
    int *count)

int MPI_Get_elements_x(const MPI_Status *status, MPI_Datatype datatype,
    MPI_Count *count)
```


## Fortran Syntax

```fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_GET_ELEMENTS(STATUS, DATATYPE, COUNT, IERROR)
    INTEGER	STATUS(MPI_STATUS_SIZE), DATATYPE, COUNT, IERROR

MPI_GET_ELEMENTS_X(STATUS, DATATYPE, COUNT, IERROR)
    INTEGER	STATUS(MPI_STATUS_SIZE), DATATYPE
        INTEGER(KIND=MPI_COUNT_KIND) COUNT
        INTEGER IERROR
```


## Fortran 2008 Syntax

```fortran
USE mpi_f08

MPI_Get_elements(status, datatype, count, ierror)
    TYPE(MPI_Status), INTENT(IN) :: status
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER, INTENT(OUT) :: count
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Get_elements_x(status, datatype, count, ierror)
    TYPE(MPI_Status), INTENT(IN) :: status
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    INTEGER(KIND = MPI_COUNT_KIND), INTENT(OUT) :: count
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```


# Input Parameters

* status : Return status of receive operation (status).
* datatype : Datatype used by receive operation (handle).

# Output Parameters

* IERROR : Fortran only: Error status (integer).

# Description

MPI_Get_elements and MPI_Get_elements_x behave different from
MPI_Get_count, which returns the number of "top-level entries"
received, i.e., the number of "copies" of type datatype. MPI_Get_count
may return any integer value k, where 0 =< k =< count. If
MPI_Get_count returns k, then the number of basic elements received (and
the value returned by MPI_Get_elements and MPI_Get_elements_x) is n
k, where n is the number of basic elements in the type map of datatype.
If the number of basic elements received is not a multiple of n, that
is, if the receive operation has not received an integral number of
datatype "copies," then MPI_Get_count returns the value MPI_UNDEFINED.
For both functions, if the count parameter cannot express the value to
be returned (e.g., if the parameter is too small to hold the output
value), it is set to MPI_UNDEFINED.

Example: Usage of MPI_Get_count and MPI_Get_element:

fortran
//...
MPI_TYPE_CONTIGUOUS(2, MPI_REAL, Type2, ierr)
MPI_TYPE_COMMIT(Type2, ierr)
//      ...
MPI_COMM_RANK(comm, rank, ierr)
IF(rank.EQ.0) THEN
    CALL MPI_SEND(a, 2, MPI_REAL, 1, 0, comm, ierr)
    CALL MPI_SEND(a, 3, MPI_REAL, 1, 0, comm, ierr)
ELSE
    CALL MPI_RECV(a, 2, Type2, 0, 0, comm, stat, ierr)
    CALL MPI_GET_COUNT(stat, Type2, i, ierr)     ! returns i=1
    CALL MPI_GET_ELEMENTS(stat, Type2, i, ierr)  ! returns i=2
    CALL MPI_RECV(a, 2, Type2, 0, 0, comm, stat, ierr)
    CALL MPI_GET_COUNT(stat, Type2, i, ierr) ! returns i=MPI_UNDEFINED

    CALL MPI_GET_ELEMENTS(stat, Type2, i, ierr)  ! returns i=3
END IF


The function MPI_Get_elements can also be used after a probe to find the
number of elements in the probed message. Note that the two functions
MPI_Get_count and MPI_Get_elements return the same values when they are
used with primitive data types.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
MPI_Comm_set_errhandler; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

# Fortran 77 Notes

The MPI standard prescribes portable Fortran syntax for the COUNT
argument of MPI_Get_elements_x only for Fortran 90. FORTRAN 77 users may
use the non-portable syntax

Fortran
INTEGER*MPI_COUNT_KIND COUNT


where MPI_COUNT_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.

# See Also

MPI_Get_count(3)
