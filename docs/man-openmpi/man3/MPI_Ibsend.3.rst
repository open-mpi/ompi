.. _mpi_ibsend:


MPI_Ibsend
==========

.. include_body

:ref:`MPI_Ibsend` |mdash| Starts a nonblocking buffered send.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Ibsend(const void *buf, int count, MPI_Datatype datatype,
   	int dest, int tag, MPI_Comm comm, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_IBSEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Ibsend(buf, count, datatype, dest, tag, comm, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: count, dest, tag
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``buf``: Initial address of send buffer (choice).
* ``count``: Number of elements in send buffer (integer).
* ``datatype``: Data type of each send buffer element (handle).
* ``dest``: Rank of destination (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Ibsend` posts a buffered-mode, nonblocking send. Nonblocking calls
allocate a communication request object and associate it with the
request handle (the argument request). The request can be used later to
query the status of the communication or wait for its completion.

A nonblocking send call indicates that the system may start copying data
out of the send buffer. The sender should not modify any part of the
send buffer after a nonblocking send operation is called, until the send
completes.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Test`
   * :ref:`MPI_Wait`
