.. _mpi_win_call_errhandler:


MPI_Win_call_errhandler
=======================

.. include_body

:ref:`MPI_Win_call_errhandler` |mdash| Passes the supplied error code to the
error handler assigned to a window


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_call_errhandler(MPI_Win win, int errorcode)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_CALL_ERRHANDLER(WIN, ERRORCODE, IERROR)
   	INTEGER	WIN, ERRORCODE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_call_errhandler(win, errorcode, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, INTENT(IN) :: errorcode
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


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
