# Name

MPI_Comm_accept - Establishes communication with a client.

# Syntax

## C Syntax

```C
#include <mpi.h>

int MPI_Comm_accept(const char *port_name, MPI_Info info, int root, MPI_Comm comm, MPI_Comm *newcomm)
```


## Fortran Syntax

```Fortran
USE MPI
! or the older form: INCLUDE 'mpif.h'

MPI_COMM_ACCEPT(PORT_NAME, INFO, ROOT, COMM, NEWCOMM, IERROR)
    CHARACTER*(*)	PORT_NAME
    INTEGER		INFO, ROOT, COMM, NEWCOMM, IERROR
```


## Fortran 2008 Syntax

```Fortran
USE mpi_f08

MPI_Comm_accept(port_name, info, root, comm, newcomm, ierror)
    CHARACTER(LEN=*), INTENT(IN) :: port_name
    TYPE(MPI_Info), INTENT(IN) :: info
    INTEGER, INTENT(IN) :: root
    TYPE(MPI_Comm), INTENT(IN) :: comm
    TYPE(MPI_Comm), INTENT(OUT) :: newcomm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```



# Input Parameters

* port_name : Port name (string, used only on *root*).
* info : Options given by root for the accept (handle, used only on root). No
options currently supported.
* root : Rank in *comm* of root node (integer).
* comm : Intracommunicator over which call is collective (handle).

# Output Parameters

* newcomm : Intercommunicator with client as remote group (handle)
* IERROR : Fortran only: Error status (integer).

# Description

MPI_Comm_accept establishes communication with a client. It is
collective over the calling communicator. It returns an
intercommunicator that allows communication with the client, after the
client has connected with the MPI_Comm_accept function using the
MPI_Comm_connect function.
The port_name must have been established through a call to
MPI_Open_port on the root.

# Errors

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.
Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
MPI_Comm_set_errhandler; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned.
See the MPI man page for a full list of MPI error codes.

# See Also

[MPI_Comm_connect(3)](MPI_Comm_connect.html)
[MPI_Open_port(3)](MPI_Open_port.html)
