.. _mpi_win_complete:


MPI_Win_complete
================

.. include_body

:ref:`MPI_Win_complete` |mdash| Completes an RMA access epoch on *win* started by
a call to :ref:`MPI_Win_start`

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_complete.rst

INPUT PARAMETERS
----------------
* ``win``: Window object (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_complete` is a one-sided MPI communication synchronization call,
completing an RMA access epoch on *win* started by a call to
:ref:`MPI_Win_start`. :ref:`MPI_Win_complete` enforces the completion of preceding RMA
calls at the origin and not at the target. A put or accumulate call may
not have completed at the target when it has completed at the origin.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_start`
