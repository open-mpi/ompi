.. _mpi_win_flush:


MPI_Win_flush
=============

.. include_body

:ref:`MPI_Win_flush`, :ref:`MPI_Win_flush_all` - Complete all outstanding RMA
operations at both the origin and the target

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Win_flush, MPI_Win_flush_all

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_flush.rst

INPUT PARAMETERS
----------------
* ``rank``: Rank of window (nonnegative integer).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_flush` completes all outstanding RMA operations initiated by
the calling process to the target rank on the specified window. The
operations are completed both at the origin and at the target.
:ref:`MPI_Win_flush_all` completes all outstanding RMA operations to all
targets.

Can only be called from within a passive target epoch.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_flush_local`
   * :ref:`MPI_Win_lock`
   * :ref:`MPI_Win_lock_all`
