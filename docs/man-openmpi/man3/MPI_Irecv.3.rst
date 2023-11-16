.. _mpi_irecv:


MPI_Irecv
=========

.. include_body

:ref:`MPI_Irecv` |mdash| Starts a standard-mode, nonblocking receive.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Irecv(void *buf, int count, MPI_Datatype datatype,
           int source, int tag, MPI_Comm comm, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_IRECV(BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST,
   		IERROR)
   	<type>	BUF(*)
   	INTEGER	COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror)
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: count, source, tag
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``buf``: Initial address of receive buffer (choice).
* ``count``: Number of elements in receive buffer (integer).
* ``datatype``: Datatype of each receive buffer element (handle).
* ``source``: Rank of source (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Nonblocking calls allocate a communication request object and associate
it with the request handle (the argument request). The request can be
used later to query the status of the communication or wait for its
completion.

A nonblocking receive call indicates that the system may start writing
data into the receive buffer. The receiver should not access any part of
the receive buffer after a nonblocking receive operation is called,
until the receive completes.

A receive request can be determined being completed by calling the
:ref:`MPI_Wait`, :ref:`MPI_Waitany`, :ref:`MPI_Test`, or :ref:`MPI_Testany` with request returned by
this function.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Recv`
   * :ref:`MPI_Probe`
   * :ref:`MPI_Test`
   * :ref:`MPI_Testany`
   * :ref:`MPI_Wait`
   * :ref:`MPI_Waitany`
