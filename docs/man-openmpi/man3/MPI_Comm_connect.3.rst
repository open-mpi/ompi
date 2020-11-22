.. _mpi_comm_connect:

MPI_Comm_connect
================

.. include_body

:ref:`MPI_Comm_connect` - Establishes communication with a server.

Syntax
------

C Syntax
^^^^^^^^

.. code:: C

   #include <mpi.h>

   int MPI_Comm_connect(const char *port_name, MPI_Info info, int root,
       MPI_Comm comm, MPI_Comm *newcomm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_CONNECT(PORT_NAME, INFO, ROOT, COMM, NEWCOMM, IERROR)
       CHARACTER*(*)   PORT_NAME
       INTEGER     INFO, ROOT, COMM, NEWCOMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: Fortran

   USE mpi_f08

   MPI_Comm_connect(port_name, info, root, comm, newcomm, ierror)
       CHARACTER(LEN=*), INTENT(IN) :: port_name
       TYPE(MPI_Info), INTENT(IN) :: info
       INTEGER, INTENT(IN) :: root
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Comm), INTENT(OUT) :: newcomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  port_name : Port name (string, used only on *root*).
-  info : Options given by root for the connect (handle, used only on
   root). No options currently supported.
-  root : Rank in *comm* of root node (integer).
-  comm : Intracommunicator over which call is collective (handle).

Output Parameters
-----------------

-  newcomm : Intercommunicator with client as remote group (handle)
-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Comm_connect` establishes communication with a server specified by
port_name. It is collective over the calling communicator and returns an
intercommunicator in which the remote group participated in an
:ref:`MPI_Comm_accept`. The :ref:`MPI_Comm_connect` call must only be called after the
:ref:`MPI_Comm_accept` call has been made by the MPI job acting as the server.
If the named port does not exist (or has been closed), :ref:`MPI_Comm_connect`
raises an error of class MPI_ERR_PORT. MPI provides no guarantee of
fairness in servicing connection attempts. That is, connection attempts
are not necessarily satisfied in the order in which they were initiated,
and competition from other connection attempts may prevent a particular
connection attempt from being satisfied. The port_name parameter is the
address of the server. It must be the same as the name returned by
:ref:`MPI_Open_port` on the server.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with :ref:`MPI_Comm_set_errhandler`;
the predefined error handler MPI_ERRORS_RETURN may be used to cause
error values to be returned. Note that MPI does not guarantee that an
MPI program can continue past an error. See the MPI man page for a full
list of MPI error codes.


.. seealso:: :ref:`MPI_Comm_accept`
