.. _mpi_win_get_name:


MPI_Win_get_name
================

.. include_body

:ref:`MPI_Win_get_name` |mdash| Obtains the name of a window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_get_name(MPI_Win win, char *win_name, int *resultlen)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_GET_NAME(WIN, WIN_NAME, RESULTLEN, IERROR)
   	INTEGER WIN, RESULTLEN, IERROR
   	CHARACTER*(*) WIN_NAME


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_get_name(win, win_name, resultlen, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: win_name
   	INTEGER, INTENT(OUT) :: resultlen
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``win``: Window whose name is to be returned (handle).

OUTPUT PARAMETERS
-----------------
* ``win_name``: the name previously stored on the window, or an empty string if no such name exists (string).
* ``resultlen``: Length of returned name (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------


ERRORS
------

.. include:: ./ERRORS.rst
