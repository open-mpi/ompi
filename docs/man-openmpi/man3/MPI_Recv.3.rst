.. _mpi_recv:


MPI_Recv
========

.. include_body

:ref:`MPI_Recv` - Performs a standard-mode blocking receive.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Recv(void *buf, int count, MPI_Datatype datatype,
   	int source, int tag, MPI_Comm comm, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_RECV(BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)
   	<type>	BUF(*)
   	INTEGER	COUNT, DATATYPE, SOURCE, TAG, COMM
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror)
   	TYPE(*), DIMENSION(..) :: buf
   	INTEGER, INTENT(IN) :: count, source, tag
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: Maximum number of elements to receive (integer).
* ``datatype``: Datatype of each receive buffer entry (handle).
* ``source``: Rank of source (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of receive buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This basic receive operation, :ref:`MPI_Recv`, is blocking: it returns only
after the receive buffer contains the newly received message. A receive
can complete before the matching send has completed (of course, it can
complete only after the matching send has started).

The blocking semantics of this call are described in Section 3.4 of the
MPI-1 Standard, "Communication Modes."

The receive buffer contains a number (defined by the value of *count*)
of consecutive elements. The first element in the set of elements is
located at *address_buf*. The type of each of these elements is
specified by *datatype*.

The length of the received message must be less than or equal to the
length of the receive buffer. An MPI_ERR_TRUNCATE is returned upon the
overflow condition.

If a message that is shorter than the length of the receive buffer
arrives, then only those locations corresponding to the (shorter)
received message are modified.


NOTES
-----

The *count* argument indicates the maximum number of entries of type
*datatype* that can be received in a message. Once a message is
received, use the :ref:`MPI_Get_count` function to determine the actual number
of entries within that message.

To receive messages of unknown length, use the :ref:`MPI_Probe` function. (For
more information about :ref:`MPI_Probe` and :ref:`MPI_Cancel`, see their respective
man pages; also, see Section 3.8 of the MPI-1 Standard, "Probe and
Cancel.")

A message can be received by a receive operation only if it is addressed
to the receiving process, and if its source, tag, and communicator
(comm) values match the source, tag, and comm values specified by the
receive operation. The receive operation may specify a wildcard value
for source and/or tag, indicating that any source and/or tag are
acceptable. The wildcard value for source is source = ``MPI_ANY_SOURCE``.
The wildcard value for tag is tag = ``MPI_ANY_TAG``. There is no wildcard
value for comm. The scope of these wildcards is limited to the processes
in the group of the specified communicator.

The message tag is specified by the tag argument of the receive
operation.

The argument source, if different from ``MPI_ANY_SOURCE``, is specified as a
rank within the process group associated with that same communicator
(remote process group, for intercommunicators). Thus, the range of valid
values for the source argument is {0,...,n-1} {``MPI_ANY_SOURCE``}, where n
is the number of processes in this group.

Note the asymmetry between send and receive operations: A receive
operation may accept messages from an arbitrary sender; on the other
hand, a send operation must specify a unique receiver. This matches a
"push" communication mechanism, where data transfer is effected by the
sender (rather than a "pull" mechanism, where data transfer is effected
by the receiver).

Source = destination is allowed, that is, a process can send a message
to itself. However, it is not recommended for a process to send messages
to itself using the blocking send and receive operations described
above, since this may lead to deadlock. See Section 3.5 of the MPI-1
Standard, "Semantics of Point-to-Point Communication."

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUS_IGNORE`` as a
special value for the *status* argument.


ERRORS
------

.. include:: ./ERRORS.rst

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on messages received
by :ref:`MPI_Recv` do not set the ``status.MPI_ERROR`` field in the
returned *status*.  The error code is always passed to the back-end
error handler and may be passed back to the caller through the return
value of :ref:`MPI_Recv` if the back-end error handler returns it.
The pre-defined MPI error handler ``MPI_ERRORS_RETURN`` exhibits this
behavior, for example.

.. seealso::
   * :ref:`MPI_Irecv`
   * :ref:`MPI_Probe`
