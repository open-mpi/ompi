.. _mpi_win_set_name:


MPI_Win_set_name
================

.. include_body

:ref:`MPI_Win_set_name` |mdash| Sets the name of a window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_set_name(MPI_Win win, const char *win_name)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_SET_NAME(WIN, WIN_NAME, IERROR)
   	INTEGER WIN, IERROR
   	CHARACTER*(*) WIN_NAME


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_set_name(win, win_name, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	CHARACTER(LEN=*), INTENT(IN) :: win_name
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``win``: Window whose identifier is to be set (handle).

INPUT PARAMETER
---------------
* ``win_name``: The character string used as the name (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------


ERRORS
------

.. include:: ./ERRORS.rst
