.. _mpi_barrier:

MPI_Barrier
===========

.. include_body

:ref:`MPI_Barrier`, :ref:`MPI_Ibarrier`, :ref:`MPI_Barrier_init` - Synchronization between MPI processes in a
group

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Barrier, MPI_Ibarrier, MPI_Barrier_init

.. The following file was automatically generated
.. include:: ./bindings/mpi_barrier.rst

INPUT PARAMETER
---------------

* ``comm`` : Communicator (handle).
* ``info`` : Info (handle, persistent only).

OUTPUT PARAMETERS
-----------------

* ``request`` : Request (handle, non-blocking and persistent only).
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
