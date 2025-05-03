.. _mpi_win_flush_local:


MPI_Win_flush_local
===================

.. include_body

:ref:`MPI_Win_flush_local`, :ref:`MPI_Win_flush_local_all` - Complete all
outstanding RMA operations at both the origin

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Win_flush_local, MPI_Win_flush_local_all

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_flush_local.rst

INPUT PARAMETERS
----------------
* ``rank``: Rank of window (nonnegative integer).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_flush_local` locally completes at the origin all outstanding
RMA operations initiated by the calling process to the target process
specified by rank on the specified window. For example, after this
routine completes, the user may reuse any buffers provided to put, get,
or accumulate operations. :ref:`MPI_Win_flush_local_all` locally completes
at the origin all outstanding RMA operations to all targets.

Can only be called from within a passive target epoch.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_flush`
   * :ref:`MPI_Win_lock`
   * :ref:`MPI_Win_lock_all`
