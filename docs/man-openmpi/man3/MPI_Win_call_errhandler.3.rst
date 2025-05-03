.. _mpi_win_call_errhandler:


MPI_Win_call_errhandler
=======================

.. include_body

:ref:`MPI_Win_call_errhandler` |mdash| Passes the supplied error code to the
error handler assigned to a window

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_call_errhandler.rst

INPUT PARAMETERS
----------------
* ``win``: Window with error handler (handle).
* ``errorcode``: MPI error code (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function invokes the error handler assigned to the window *win*
with the supplied error code *errorcode*. If the error handler was
successfully called, the process is not aborted, and the error handler
returns, this function returns MPI_SUCCESS.


NOTES
-----

Users should note that the default error handler is
MPI_ERRORS_ARE_FATAL. Thus, calling this function will abort the window
processes if the default error handler has not been changed for this
window.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_create_errhandler`
   * :ref:`MPI_Win_set_errhandler`
