.. _mpi_win_flush:


MPI_Win_flush
=============

.. include_body

:ref:`MPI_Win_flush`, :ref:`MPI_Win_flush_all` - Complete all outstanding RMA
operations at both the origin and the target


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_flush (int rank, MPI_Win win)

   int MPI_Win_flush_all (MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_FLUSH(RANK, WIN, IERROR)
   	INTEGER RANK, WIN, IERROR

   MPI_WIN_FLUSH_ALL(WIN, IERROR)
   	INTEGER WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_flush(rank, win, ierror)
   	INTEGER, INTENT(IN) :: rank
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Win_flush_all(win, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``rank``: Rank of window (nonnegative integer).
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

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

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned. Note
that MPI does not guarantee that an MPI program can continue past an
error.


.. seealso::
   :ref:`MPI_Win_flush_local` :ref:`MPI_Win_lock` :ref:`MPI_Win_lock_all`
