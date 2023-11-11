.. _mpi_win_free_keyval:


MPI_Win_free_keyval
===================

.. include_body

:ref:`MPI_Win_free_keyval` |mdash| Frees a window keyval.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_free_keyval(int *win_keyval)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_FREE_KEYVAL(WIN_KEYVAL, IERROR)
   	INTEGER WIN_KEYVAL, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_free_keyval(win_keyval, ierror)
   	INTEGER, INTENT(INOUT) :: win_keyval
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``win_keyval``: Key value (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst
