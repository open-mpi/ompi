.. _mpi_win_set_errhandler:


MPI_Win_set_errhandler
======================

.. include_body

:ref:`MPI_Win_set_errhandler` - Attaches a new error handler to a window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_SET_ERRHANDLER(WIN, ERRHANDLER, IERROR)
   	INTEGER WIN, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_set_errhandler(win, errhandler, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``win``: Window (handle).

INPUT PARAMETER
---------------
* ``errhandler``: New error handler for window (handle).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_set_errhandler` attaches a new error handler to a window. The
error handler must be either a predefined error handler or an error
handler created by a call to :ref:`MPI_Win_create_errhandler`.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
