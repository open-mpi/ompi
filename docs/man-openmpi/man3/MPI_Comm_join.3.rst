.. _mpi_comm_join:


MPI_Comm_join
=============

.. include_body

:ref:`MPI_Comm_join` |mdash| Establishes communication between MPI jobs


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_join(int fd, MPI_Comm *intercomm)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_JOIN(FD, INTERCOMM, IERROR)
   	INTEGER	FD, INTERCOMM, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_join(fd, intercomm, ierror)
   	INTEGER, INTENT(IN) :: fd
   	TYPE(MPI_Comm), INTENT(OUT) :: intercomm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``fd``: socket file descriptor (socket).

OUTPUT PARAMETERS
-----------------
* ``intercomm``: Intercommunicator between processes (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_join` creates an intercommunicator from the union of two MPI
processes that are connected by a socket. *fd* is a file descriptor
representing a socket of type SOCK_STREAM (a two-way reliable
byte-stream connection). Nonblocking I/O and asynchronous notification
via SIGIO must not be enabled for the socket. The socket must be in a
connected state, and must be quiescent when :ref:`MPI_Comm_join` is called.

:ref:`MPI_Comm_join` must be called by the process at each end of the socket.
It does not return until both processes have called :ref:`MPI_Comm_join`.


NOTES
-----

There are no MPI library calls for opening and manipulating a socket.
The socket *fd* can be opened using standard socket API calls. MPI uses
the socket to bootstrap creation of the intercommunicator, and for
nothing else. Upon return, the file descriptor will be open and
quiescent.

In a multithreaded process, the application must ensure that other
threads do not access the socket while one is in the midst of calling
:ref:`MPI_Comm_join`.

The returned communicator will contain the two processes connected by
the socket, and may be used to establish MPI communication with
additional processes, through the usual MPI communicator-creation
mechanisms.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * socket(3SOCKET)
   * :ref:`MPI_Comm_create`
   * :ref:`MPI_Comm_group`
