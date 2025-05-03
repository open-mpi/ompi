.. _mpi_win_unlock_all:


MPI_Win_unlock_all
==================

.. include_body

:ref:`MPI_Win_unlock_all` |mdash| Completes an RMA access epoch started by a call
to :ref:`MPI_Win_lock_all`.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_unlock_all.rst

INPUT PARAMETERS
----------------
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_unlock_all` completes an RMA access epoch started by a call to
:ref:`MPI_Win_lock_all`. RMA operations issued during this period will have
completed both at the origin and at the target when the call returns.

Locks are used to protect accesses to the locked target window effected
by RMA calls issued between the lock and unlock call, and to protect
local load/store accesses to a locked local window executed between the
lock and unlock call. Accesses that are protected by an exclusive lock
will not be concurrent at the window site with other accesses to the
same window that are lock protected. Accesses that are protected by a
shared lock will not be concurrent at the window site with accesses
protected by an exclusive lock to the same window.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_lock_all`
   * :ref:`MPI_Win_unlock`
