.. _mpi_comm_disconnect:

MPI_Comm_disconnect
===================

.. include_body

:ref:`MPI_Comm_disconnect` - Deallocates communicator object and sets handle to
MPI_COMM_NULL.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Comm_disconnect(MPI_Comm *comm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_DISCONNECT(COMM, IERROR)
       INTEGER COMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Comm_disconnect(comm, ierror)
       TYPE(MPI_Comm), INTENT(INOUT) :: comm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input/Output Parameter
^^^^^^^^^^^^^^^^^^^^^^

-  comm : Communicator (handle).

Output Parameter
----------------

-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Comm_disconnect` waits for all pending communication on comm to
complete internally, deallocates the communicator object, and sets the
handle to MPI_COMM_NULL. It is a collective operation. It may not be
called with the communicator MPI_COMM_WORLD or MPI_COMM_SELF.
:ref:`MPI_Comm_disconnect` may be called only if all communication is complete
and matched, so that buffered data can be delivered to its destination.
This requirement is the same as for :ref:`MPI_Finalize`. :ref:`MPI_Comm_disconnect`
has the same action as :ref:`MPI_Comm_free`, except that it waits for pending
communication to finish internally and enables the guarantee about the
behavior of disconnected processes.

Notes
-----

To disconnect two processes you may need to call :ref:`MPI_Comm_disconnect`,
:ref:`MPI_Win_free`, and :ref:`MPI_File_close` to remove all communication paths
between the two processes. Note that it may be necessary to disconnect
several communicators (or to free several windows or files) before two
processes are completely independent.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with :ref:`MPI_Comm_set_errhandler`;
the predefined error handler MPI_ERRORS_RETURN may be used to cause
error values to be returned. Note that MPI does not guarantee that an
MPI program can continue past an error.


.. seealso:: :ref:`MPI_Comm_connect`
