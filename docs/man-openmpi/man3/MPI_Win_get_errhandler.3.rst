.. _mpi_win_get_errhandler:


MPI_Win_get_errhandler
======================

.. include_body

:ref:`MPI_Win_get_errhandler` |mdash| Retrieves the error handler currently
associated with a window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_GET_ERRHANDLER(WIN, ERRHANDLER, IERROR)
   	INTEGER WIN, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_get_errhandler(win, errhandler, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``win``: Window (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: Error handler currently associated with window (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_get_errhandler` retrieves the error handler currently associated
with a window.


ERRORS
------

.. include:: ./ERRORS.rst
