.. _mpi_comm_connect:

MPI_Comm_connect
================

.. include_body

:ref:`MPI_Comm_connect` |mdash| Establishes communication with a server.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Comm_connect(const char *port_name, MPI_Info info, int root,
       MPI_Comm comm, MPI_Comm *newcomm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_CONNECT(PORT_NAME, INFO, ROOT, COMM, NEWCOMM, IERROR)
       CHARACTER*(*)   PORT_NAME
       INTEGER     INFO, ROOT, COMM, NEWCOMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Comm_connect(port_name, info, root, comm, newcomm, ierror)
       CHARACTER(LEN=*), INTENT(IN) :: port_name
       TYPE(MPI_Info), INTENT(IN) :: info
       INTEGER, INTENT(IN) :: root
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Comm), INTENT(OUT) :: newcomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``port_name`` : Port name (string, used only on *root*).
* ``info`` : Options given by root for the connect (handle, used only on
   root). No options currently supported.
* ``root`` : Rank in *comm* of root node (integer).
* ``comm`` : Intracommunicator over which call is collective (handle).

OUTPUT PARAMETERS
-----------------

* ``newcomm`` : Intercommunicator with client as remote group (handle)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
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

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_accept`
