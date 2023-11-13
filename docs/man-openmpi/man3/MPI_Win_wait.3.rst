.. _mpi_win_wait:


MPI_Win_wait
============

.. include_body

:ref:`MPI_Win_wait` |mdash| Completes an RMA exposure epoch started by a call to
:ref:`MPI_Win_post` on *win*


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_wait(MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_WAIT( WIN, IERROR)
   	INTEGER  WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_wait(win, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win``: Window object (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_wait` is a one-sided MPI communication synchronization call that
completes an RMA exposure epoch started by a call to :ref:`MPI_Win_post` on
*win*. This call matches calls to MPI_Win_complete(win) issued by each
of the processes that were granted access to the window during this
epoch. The call to :ref:`MPI_Win_wait` blocks until all matching calls to
:ref:`MPI_Win_complete` have occurred. This guarantees that all these origin
processes have completed their RMA accesses to the local window. When
the call returns, all these RMA accesses will have completed at the
target window.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_post`
