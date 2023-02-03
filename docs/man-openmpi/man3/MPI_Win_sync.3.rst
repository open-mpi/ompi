.. _mpi_win_sync:


MPI_Win_sync
============

.. include_body

:ref:`MPI_Win_sync`, - Synchronize the private and public copies of the
window


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_sync (MPI_Win win)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_SYNC(WIN, IERROR)
   	INTEGER WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_sync(win, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win``: Window object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_sync` synchronizes the private and public window copies of
*win*. For the purposes of synchronizing the private and public window,
:ref:`MPI_Win_sync` has the effect of ending and reopening an access and
exposure epoch on the window (note that it does not actually end an
epoch or complete any pending MPI RMA operations).


ERRORS
------

.. include:: ./ERRORS.rst
