.. _mpi_win_complete:


MPI_Win_complete
================

.. include_body

:ref:`MPI_Win_complete` - Completes an RMA access epoch on *win* started by
a call to :ref:`MPI_Win_start`


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   MPI_Win_complete(MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_COMPLETE(WIN, IERROR)


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_complete(win, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win``: Window object (handle).

OUTPUT PARAMETERS
-----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_complete` is a one-sided MPI communication synchronization call,
completing an RMA access epoch on *win* started by a call to
:ref:`MPI_Win_start`. :ref:`MPI_Win_complete` enforces the completion of preceding RMA
calls at the origin and not at the target. A put or accumulate call may
not have completed at the target when it has completed at the origin.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Win_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Win_start`
