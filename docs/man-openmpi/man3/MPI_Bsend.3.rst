.. _mpi_bsend:

MPI_Bsend
=========

.. include_body

:ref:`MPI_Bsend` - Basic send with user-specified buffering.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Bsend(const void *buf, int count, MPI_Datatype datatype,
      int dest, int tag, MPI_Comm comm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_BSEND(BUF, COUNT,DATATYPE, DEST, TAG, COMM, IERROR)
       <type>  BUF(*)
       INTEGER COUNT, DATATYPE, DEST, TAG, COMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08
   MPI_Bsend(buf, count, datatype, dest, tag, comm, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN) :: buf
       INTEGER, INTENT(IN) :: count, dest, tag
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  ``buf`` : Initial address of send buffer (choice).
-  ``count`` : Number of entries in send buffer (nonnegative integer).
-  ``datatype`` : Datatype of each send buffer element (handle).
-  ``dest`` : Rank of destination (integer).
-  ``tag`` : Message tag (integer).
-  ``comm`` : Communicator (handle).

Output Parameters
-----------------

-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Bsend` performs a buffered-mode, blocking send.

Notes
-----

This send is provided as a convenience function; it allows the user to
send messages without worrying about where they are buffered (because
the user must have provided buffer space with :ref:`MPI_Buffer_attach`).

In deciding how much buffer space to allocate, remember that the buffer
space is not available for reuse by subsequent :ref:`MPI_Bsend`\ s unless
you are certain that the message has been received (not just that it
should have been received). For example, this code does not allocate
enough buffer space:

.. code:: c

   MPI_Buffer_attach( b, n*sizeof(double) + MPI_BSEND_OVERHEAD );
   for (i=0; i<m; i++) {
       MPI_Bsend( buf, n, MPI_DOUBLE, ... );
   }

because only enough buffer space is provided for a single send, and the
loop may start a second ``MPI_Bsend`` before the first is done making
use of the buffer.

In C, you can force the messages to be delivered by
``MPI_Buffer_detach( &b, &n );`` ``MPI_Buffer_attach( b, n );`` (The
``MPI_Buffer_detach`` will not complete until all buffered messages are
delivered.)

Errors
------

Almost all MPI routines return an error value; C routines as the value;
C routines as the value of the function and Fortran routines in the last
argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
