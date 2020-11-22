.. _mpi_barrier:

MPI_Barrier
===========

.. include_body

:ref:`MPI_Barrier`, :ref:`MPI_Ibarrier` - Synchronization between MPI processes in a
group

Syntax
------

C Syntax
^^^^^^^^

.. code:: C

   #include <mpi.h>

   int MPI_Barrier(MPI_Comm)
   int MPI_Ibarrier(MPI_Comm comm, MPI_Request *request)
   int MPI_barrier_init(MPI_Comm comm, MPI_Info info, MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_BARRIER(COMM, IERROR)
       INTEGER COMM, IERROR
   MPI_IBARRIER(COMM, REQUEST, IERROR)
       INTEGER COMM, REQUEST, IERROR
   MPI_BARRIER_INIT(COMM, INFO, REQUEST, IERROR)
       INTEGER COMM, INFO, REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: Fortran

   USE mpi_f08
   MPI_Barrier(comm, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   MPI_Ibarrier(comm, request, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Request), INTENT (OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   MPI_Barrier_init(comm, info, request, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Info), INTENT(IN) :: info
       TYPE(MPI_Request), INTENT (OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameter
---------------

-  comm : Communicator (handle).
-  info : Info (handle, persistent only).

Output Parameters
-----------------

-  request : Request (handle, non-blocking only).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

An MPI barrier completes after all groups members have entered the
barrier.

When Communicator is an Inter-Communicator
------------------------------------------

When the communicator is an inter-communicator, the barrier operation is
performed across all processes in both groups. All processes in the
first group may exit the barrier when all processes in the second group
have entered the barrier.

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


.. seealso:: :ref:`MPI_Bcast`
