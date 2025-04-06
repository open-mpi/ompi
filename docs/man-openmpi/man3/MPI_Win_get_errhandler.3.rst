.. _mpi_win_get_errhandler:


MPI_Win_get_errhandler
======================

.. include_body

:ref:`MPI_Win_get_errhandler` |mdash| Retrieves the error handler currently
associated with a window.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_get_errhandler.rst

INPUT PARAMETER
---------------
* ``win``: Window (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: Error handler currently associated with window (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_get_errhandler` retrieves the error handler currently associated
with a window.


ERRORS
------

.. include:: ./ERRORS.rst
