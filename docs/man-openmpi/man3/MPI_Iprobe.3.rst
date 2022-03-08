.. _mpi_iprobe:


MPI_Iprobe
==========

.. include_body

:ref:`MPI_Iprobe` - Nonblocking test for a message.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag,
   	MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_IPROBE(SOURCE, TAG, COMM, FLAG, STATUS, IERROR)
   	LOGICAL	FLAG
   	INTEGER	SOURCE, TAG, COMM, STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Iprobe(source, tag, comm, flag, status, ierror)
   	INTEGER, INTENT(IN) :: source, tag
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	LOGICAL, INTENT(OUT) :: flag
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``source``: Source rank or MPI_ANY_SOURCE (integer).
* ``tag``: Tag value or MPI_ANY_TAG (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``flag``: Message-waiting flag (logical).
* ``status``: Status object (status).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The :ref:`MPI_Probe` and :ref:`MPI_Iprobe` operations allow checking of incoming
messages without actual receipt of them. The user can then decide how to
receive them, based on the information returned by the probe (basically,
the information returned by status). In particular, the user may
allocate memory for the receive buffer, according to the length of the
probed message.

MPI_Iprobe(source, tag, comm, flag, status) returns flag = true if there
is a message that can be received and that matches the pattern specified
by the arguments source, tag, and comm. The call matches the same
message that would have been received by a call to MPI_Recv(..., source,
tag, comm, status) executed at the same point in the program, and
returns in status the same value that would have been returned by
MPI_Recv(). Otherwise, the call returns flag = false, and leaves status
undefined.

If :ref:`MPI_Iprobe` returns flag = true, then the content of the status object
can be subsequently accessed as described in Section 3.2.5 of the MPI-1
Standard, "Return Status," to find the source, tag, and length of the
probed message.

A subsequent receive executed with the same context, and the source and
tag returned in status by :ref:`MPI_Iprobe` will receive the message that was
matched by the probe if no other intervening receive occurs after the
probe. If the receiving process is multithreaded, it is the user's
responsibility to ensure that the last condition holds.

The source argument of :ref:`MPI_Probe` can be MPI_ANY_SOURCE, and the tag
argument can be MPI_ANY_TAG, so that one can probe for messages from an
arbitrary source and/or with an arbitrary tag. However, a specific
communication context must be provided with the comm argument.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant MPI_STATUS_IGNORE as a
special value for the *status* argument.

It is not necessary to receive a message immediately after it has been
probed for, and the same message may be probed for several times before
it is received.


NOTE
----

Users of libmpi-mt should remember that two threads may do an :ref:`MPI_Iprobe`
that actually returns true for the same message for both threads.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Probe` :ref:`MPI_Cancel`
