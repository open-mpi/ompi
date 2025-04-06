.. _mpi_win_set_errhandler:


MPI_Win_set_errhandler
======================

.. include_body

:ref:`MPI_Win_set_errhandler` |mdash| Attaches a new error handler to a window.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_set_errhandler.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``win``: Window (handle).

INPUT PARAMETER
---------------
* ``errhandler``: New error handler for window (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_set_errhandler` attaches a new error handler to a window. The
error handler must be either a predefined error handler or an error
handler created by a call to :ref:`MPI_Win_create_errhandler`.


ERRORS
------

.. include:: ./ERRORS.rst
