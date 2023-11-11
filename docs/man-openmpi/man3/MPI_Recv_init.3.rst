.. _mpi_recv_init:


MPI_Recv_init
=============

.. include_body

:ref:`MPI_Recv_init` |mdash| Builds a handle for a receive.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Recv_init(void *buf, int count, MPI_Datatype datatype,
   	int source, int tag, MPI_Comm comm, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_RECV_INIT(BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST,
   		IERROR)
   	<type>	BUF(*)
   	INTEGER	COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierror)
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: count, source, tag
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: Maximum number of elements to receive (integer).
* ``datatype``: Type of each entry (handle).
* ``source``: Rank of source (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

INPUT/OUTPUT PARAMETER
----------------------
* ``buf``: Initial address of receive buffer (choice).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Creates a persistent communication request for a receive operation. The
argument *buf* is marked as OUT because the user gives permission to
write on the receive buffer by passing the argument to :ref:`MPI_Recv_init`.

A persistent communication request is inactive after it is created -- no
active communication is attached to the request.

A communication (send or receive) that uses a persistent request is
initiated by the function :ref:`MPI_Start` or :ref:`MPI_Startall`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Bsend_init`
   * :ref:`MPI_Rsend_init`
   * :ref:`MPI_Send_init`
   * MPI_Sssend_init
   * :ref:`MPI_Start`
   * :ref:`MPI_Startall`
   * :ref:`MPI_Request_free`
