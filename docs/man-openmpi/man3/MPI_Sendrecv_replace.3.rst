.. _mpi_sendrecv_replace:


MPI_Sendrecv_replace
====================

.. include_body

:ref:`MPI_Sendrecv_replace` - Sends and receives a message using a single
buffer.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Sendrecv_replace(void *buf, int count, MPI_Datatype datatype,
   	int dest, int sendtag, int source, int recvtag, MPI_Comm comm,
   	MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SENDRECV_REPLACE(BUF, COUNT, DATATYPE, DEST, SENDTAG, SOURCE,
   		RECVTAG, COMM, STATUS, IERROR)
   	<type>	BUF(*)
   	INTEGER	COUNT, DATATYPE, DEST, SENDTAG
   	INTEGER	SOURCE, RECVTAG, COMM
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Sendrecv_replace(buf, count, datatype, dest, sendtag, source, recvtag,
   		comm, status, ierror)
   	TYPE(*), DIMENSION(..) :: buf
   	INTEGER, INTENT(IN) :: count, dest, sendtag, source, recvtag
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Status) :: status
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
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The send-receive operations combine in one call the sending of a message
to one destination and the receiving of another message, from another
process. The two (source and destination) are possibly the same. A
send-receive operation is useful for executing a shift operation across
a chain of processes. If blocking sends and receives are used for such a
shift, then one needs to order the sends and receives correctly (for
example, even processes send, then receive; odd processes receive first,
then send) in order to prevent cyclic dependencies that may lead to
deadlock. When a send-receive operation is used, the communication
subsystem takes care of these issues. The send-receive operation can be
used in conjunction with the functions described in Chapter 6 of the MPI
Standard, "Process Topologies," in order to perform shifts on various
logical topologies. Also, a send-receive operation is useful for
implementing remote procedure calls.

A message sent by a send-receive operation can be received by a regular
receive operation or probed by a probe operation; a send-receive
operation can receive a message sent by a regular send operation.

:ref:`MPI_Sendrecv_replace` executes a blocking send and receive. The same
buffer is used both for the send and for the receive, so that the
message sent is replaced by the message received.

The semantics of a send-receive operation is what would be obtained if
the caller forked two concurrent threads, one to execute the send, and
one to execute the receive, followed by a join of these two threads.


ERRORS
------

.. include:: ./ERRORS.rst

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on messages received
by :ref:`MPI_Sendrecv_replace` do not set the ``status.MPI_ERROR``
field in the returned *status*.  The error code is always passed to
the back-end error handler and may be passed back to the caller
through the return value of :ref:`MPI_Sendrecv_replace` if the
back-end error handler returns it.  The pre-defined MPI error handler
``MPI_ERRORS_RETURN`` exhibits this behavior, for example.

.. seealso::
   * :ref:`MPI_Sendrecv`
