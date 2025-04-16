.. _mpi_isendrecv_replace:


MPI_Isendrecv_replace
=====================

.. include_body

:ref:`MPI_Isendrecv_replace` |mdash| Sends and receives a message using a single
buffer.

.. The following file was automatically generated
.. include:: ./bindings/mpi_isendrecv_replace.rst

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
