.. _mpi_comm_accept:

MPI_Comm_accept
===============

.. include_body

:ref:`MPI_Comm_accept` |mdash| Establishes communication with a client.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Comm_accept(const char *port_name, MPI_Info info, int root, MPI_Comm comm, MPI_Comm *newcomm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_ACCEPT(PORT_NAME, INFO, ROOT, COMM, NEWCOMM, IERROR)
       CHARACTER*(*)   PORT_NAME
       INTEGER     INFO, ROOT, COMM, NEWCOMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Comm_accept(port_name, info, root, comm, newcomm, ierror)
       CHARACTER(LEN=*), INTENT(IN) :: port_name
       TYPE(MPI_Info), INTENT(IN) :: info
       INTEGER, INTENT(IN) :: root
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Comm), INTENT(OUT) :: newcomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``port_name`` : Port name (string, used only on *root*).
* ``info`` : Options given by root for the accept (handle, used only on
   root). No options currently supported.
* ``root`` : Rank in *comm* of root node (integer).
* ``comm`` : Intracommunicator over which call is collective (handle).

OUTPUT PARAMETERS
-----------------

* ``newcomm`` : Intercommunicator with client as remote group (handle)
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_accept` establishes communication with a client. It is
collective over the calling communicator. It returns an
intercommunicator that allows communication with the client, after the
client has connected with the :ref:`MPI_Comm_accept` function using the
:ref:`MPI_Comm_connect` function. The port_name must have been established
through a call to :ref:`MPI_Open_port` on the root.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_connect`
