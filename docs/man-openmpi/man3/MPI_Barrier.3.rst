.. _mpi_barrier:

MPI_Barrier
===========

.. include_body

:ref:`MPI_Barrier`, :ref:`MPI_Ibarrier` - Synchronization between MPI processes in a
group

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Barrier(MPI_Comm)
   int MPI_Ibarrier(MPI_Comm comm, MPI_Request *request)
   int MPI_Barrier_init(MPI_Comm comm, MPI_Info info, MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

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

.. code-block:: Fortran

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

INPUT PARAMETER
---------------

* ``comm`` : Communicator (handle).
* ``info`` : Info (handle, persistent only).

OUTPUT PARAMETERS
-----------------

* ``request`` : Request (handle, non-blocking only).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

An MPI barrier completes after all groups members have entered the
barrier.

When Communicator is an Inter-Communicator
------------------------------------------

When the communicator is an inter-communicator, the barrier operation is
performed across all processes in both groups. All processes in the
first group may exit the barrier when all processes in the second group
have entered the barrier.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Bcast`
