.. _mpi_sendrecv:


MPI_Sendrecv
============

.. include_body

:ref:`MPI_Sendrecv` |mdash| Sends and receives a message.

.. The following file was automatically generated
.. include:: ./bindings/mpi_sendrecv.rst

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
* ``status``: Status object (status). This refers to the receive operation.
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
used in conjunction with the functions described in the "Process
Topologies" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_ in order to perform shifts on
various logical topologies. Also, a send-receive operation is useful for
implementing remote procedure calls.

A message sent by a send-receive operation can be received by a regular
receive operation or probed by a probe operation; a send-receive
operation can receive a message sent by a regular send operation.

:ref:`MPI_Sendrecv` executes a blocking send and receive operation. Both send
and receive use the same communicator, but possibly different tags. The
send buffer and receive buffers must be disjoint, and may have different
lengths and datatypes.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUS_IGNORE`` as a
special value for the *status* argument.


ERRORS
------

.. include:: ./ERRORS.rst

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on messages received
by :ref:`MPI_Sendrecv` do not set the ``status.MPI_ERROR`` field in
the returned *status*.  The error code is always passed to the
back-end error handler and may be passed back to the caller through
the return value of :ref:`MPI_Sendrecv` if the back-end error handler
returns it.  The pre-defined MPI error handler ``MPI_ERRORS_RETURN``
exhibits this behavior, for example.

.. seealso::
   * :ref:`MPI_Sendrecv_replace`
