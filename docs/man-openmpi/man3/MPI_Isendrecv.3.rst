.. _mpi_isendrecv:


MPI_Isendrecv
=============

.. include_body

:ref:`MPI_Isendrecv` |mdash| Sends and receives a message.

.. The following file was automatically generated
.. include:: ./bindings/mpi_isendrecv.rst

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
