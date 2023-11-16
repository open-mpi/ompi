.. _mpi_isendrecv:


MPI_Isendrecv
=============

.. include_body

:ref:`MPI_Isendrecv` |mdash| Sends and receives a message.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Isendrecv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
       int dest, int sendtag, void *recvbuf, int recvcount,
       MPI_Datatype recvtype, int source, int recvtag,
       MPI_Comm comm, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ISENDRECV(SENDBUF, SENDCOUNT, SENDTYPE, DEST, SENDTAG,
           RECVBUF, RECVCOUNT, RECVTYPE, SOURCE, RECVTAG, COMM,
           REQUEST, IERROR)
       <type>    SENDBUF(*), RECVBUF(*)
       INTEGER    SENDCOUNT, SENDTYPE, DEST, SENDTAG
       INTEGER    RECVCOUNT, RECVTYPE, SOURCE, RECVTAG, COMM
       INTEGER    REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Isendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf,
           recvcount, recvtype, source, recvtag, comm, request, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
       TYPE(*), DIMENSION(..) :: recvbuf
       INTEGER, INTENT(IN) :: sendcount, dest, sendtag, recvcount, source,
       recvtag
       TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Request) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``sendbuf``: Initial address of send buffer (choice).
* ``sendcount``: Number of elements to send (integer).
* ``sendtype``: Type of elements in send buffer (handle).
* ``dest``: Rank of destination (integer).
* ``sendtag``: Send tag (integer).
* ``recvcount``: Maximum number of elements to receive (integer).
* ``recvtype``: Type of elements in receive buffer (handle).
* ``source``: Rank of source (integer).
* ``recvtag``: Receive tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Initial address of receive buffer (choice).
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The non-blocking send-receive operations combine in one call the sending
of a message to one destination and the receiving of another message,
from another process. The two (source and destination) are possibly the
same. This operation is useful for executing a shift operation across a
chain of processes. The send-receive operation can be used in
conjunction with the functions described in the "Process Topologies"
chapter of the MPI Standard in order to perform shifts on various
logical topologies.

A message sent by a send-receive operation can be received by a regular
receive operation or probed by a probe operation; a send-receive
operation can receive a message sent by a regular send operation.

:ref:`MPI_Isendrecv` executes a non-blocking send and receive operation. Both
send and receive use the same communicator, but possibly different tags.
The send buffer and receive buffers must be disjoint, and may have
different lengths and datatypes.

A non-blocking send-receive request can be determined to be completed by
calling the :ref:`MPI_Wait`, :ref:`MPI_Waitany`, :ref:`MPI_Test`, or :ref:`MPI_Testany` with the
request returned by this function.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Isendrecv_replace`
   * :ref:`MPI_Sendrecv`
   * :ref:`MPI_Sendrecv_replace`
