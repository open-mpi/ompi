.. _mpi_probe:


MPI_Probe
=========

.. include_body

:ref:`MPI_Probe` |mdash| Blocking test for a message.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PROBE(SOURCE, TAG, COMM, STATUS, IERROR)
   	INTEGER	SOURCE, TAG, COMM, STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Probe(source, tag, comm, status, ierror)
   	INTEGER, INTENT(IN) :: source, tag
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``source``: Source rank or ``MPI_ANY_SOURCE`` (integer).
* ``tag``: Tag value or ``MPI_ANY_TAG`` (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The :ref:`MPI_Probe` and :ref:`MPI_Iprobe` operations allow checking of incoming
messages, without actual receipt of them. The user can then decide how
to receive them, based on the information returned by the probe in the
status variable. For example, the user may allocate memory for the
receive buffer, according to the length of the probed message.

:ref:`MPI_Probe` behaves like :ref:`MPI_Iprobe` except that it is a blocking call that
returns only after a matching message has been found.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUS_IGNORE`` as a
special value for the *status* argument.

The semantics of :ref:`MPI_Probe` and :ref:`MPI_Iprobe` guarantee progress: If a call
to :ref:`MPI_Probe` has been issued by a process, and a send that matches the
probe has been initiated by some process, then the call to :ref:`MPI_Probe`
will return, unless the message is received by another concurrent
receive operation (that is executed by another thread at the probing
process). Similarly, if a process busy waits with :ref:`MPI_Iprobe` and a
matching message has been issued, then the call to :ref:`MPI_Iprobe` will
eventually return *flag* = true unless the message is received by another
concurrent receive operation.

**Example 1:** Use blocking probe to wait for an incoming message.

.. code-block:: fortran

   CALL MPI_COMM_RANK(comm, rank, ierr)
   IF (rank == 0) THEN
      CALL MPI_SEND(i, 1, MPI_INTEGER, 2, 0, comm, ierr)
   ELSE IF(rank == 1) THEN
      CALL MPI_SEND(x, 1, MPI_REAL, 2, 0, comm, ierr)
   ELSE   ! rank == 2
      DO i=1, 2
         CALL MPI_PROBE(MPI_ANY_SOURCE, 0,
                        comm, status, ierr)
	 IF (status(MPI_SOURCE) = 0) THEN
	    CALL MPI_RECV(i, 1, MPI_INTEGER, 0, 0, status, ierr)
         ELSE
	    CALL MPI_RECV(x, 1, MPI_REAL, 1, 0, status, ierr)
         END IF
      END DO
   END IF

Each message is received with the right type.

**Example 2:** A program similar to the previous example, but with a
problem.

.. code-block:: fortran

   CALL MPI_COMM_RANK(comm, rank, ierr)
   IF (rank == 0) THEN
      CALL MPI_SEND(i, 1, MPI_INTEGER, 2, 0, comm, ierr)
   ELSE IF(rank == 1) THEN
      CALL MPI_SEND(x, 1, MPI_REAL, 2, 0, comm, ierr)
   ELSE
      DO i=1, 2
         CALL MPI_PROBE(MPI_ANY_SOURCE, 0,
                        comm, status, ierr)
	 IF (status(MPI_SOURCE) == 0) THEN
	    CALL MPI_RECV(i, 1, MPI_INTEGER, MPI_ANY_SOURCE, &
                          0, status, ierr)
	 ELSE
	    CALL MPI_RECV(x, 1, MPI_REAL, MPI_ANY_SOURCE, &
                          0, status, ierr)
	 END IF
      END DO
   END IF

We slightly modified Example 2, using ``MPI_ANY_SOURCE`` as the source
argument in the two receive calls in statements labeled 100 and 200. The
program is now incorrect: The receive operation may receive a message
that is distinct from the message probed by the preceding call to
:ref:`MPI_Probe`.


ERRORS
------

.. include:: ./ERRORS.rst

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on messages queried
by :ref:`MPI_Probe` do not set the ``status.MPI_ERROR`` field in the
returned *status*.  The error code is always passed to the back-end
error handler and may be passed back to the caller through the return
value of :ref:`MPI_Probe` if the back-end error handler returns it.
The pre-defined MPI error handler ``MPI_ERRORS_RETURN`` exhibits this
behavior, for example.

.. seealso::
   * :ref:`MPI_Iprobe`
   * :ref:`MPI_Cancel`
