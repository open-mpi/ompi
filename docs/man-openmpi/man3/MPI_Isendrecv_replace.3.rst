.. _mpi_isendrecv_replace:


MPI_Isendrecv_replace
=====================

.. include_body

:ref:`MPI_Isendrecv_replace` |mdash| Sends and receives a message using a single
buffer.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Isendrecv_replace(void *buf, int count, MPI_Datatype datatype,
       int dest, int sendtag, int source, int recvtag, MPI_Comm comm,
       MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ISENDRECV_REPLACE(BUF, COUNT, DATATYPE, DEST, SENDTAG, SOURCE,
           RECVTAG, COMM, REQUEST, IERROR)
       <type>    BUF(*)
       INTEGER    COUNT, DATATYPE, DEST, SENDTAG
       INTEGER    SOURCE, RECVTAG, COMM
       INTEGER    REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Isendrecv_replace(buf, count, datatype, dest, sendtag, source, recvtag,
           comm, request, ierror)
       TYPE(*), DIMENSION(..) :: buf
       INTEGER, INTENT(IN) :: count, dest, sendtag, source, recvtag
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Request) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``buf``: Initial address of send and receive buffer (choice).

INPUT PARAMETERS
----------------
* ``count``: Number of elements in send and receive buffer (integer).
* ``datatype``: Type of elements to send and receive (handle).
* ``dest``: Rank of destination (integer).
* ``sendtag``: Send message tag (integer).
* ``source``: Rank of source (integer).
* ``recvtag``: Receive message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The non-blocking send-receive operations combine in one call the sending
of a message to one destination and the receiving of another message,
from another process. The two (source and destination) are possibly the
same. A send-receive operation is useful for executing a shift operation
across a chain of processes. The send-receive operation can be used in
conjunction with the functions described in the "Process Topologies"
chapter of the MPI Standard in order to perform shifts on various
logical topologies. Also, a send-receive operation is useful for
implementing remote procedure calls.

A message sent by a send-receive operation can be received by a regular
receive operation or probed by a probe operation; a send-receive
operation can receive a message sent by a regular send operation.

:ref:`MPI_Isendrecv_replace` executes a non-blocking send and receive. The same
buffer is used both for the send and for the receive, so that the
message sent is replaced by the message received.

A non-blocking send-receive request can be determined to be completed by
calling the :ref:`MPI_Wait`, :ref:`MPI_Waitany`, :ref:`MPI_Test`, or :ref:`MPI_Testany` with the
request returned by this function.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Isendrecv`
   * :ref:`MPI_Sendrecv`
   * :ref:`MPI_Sendrecv_replace`
