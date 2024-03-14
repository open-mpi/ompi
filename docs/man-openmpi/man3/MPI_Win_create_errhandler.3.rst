.. _mpi_win_create_errhandler:


MPI_Win_create_errhandler
=========================

.. include_body

:ref:`MPI_Win_create_errhandler` |mdash| Creates an error handler for a window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_create_errhandler(MPI_Win_errhandler_function *function,
   	MPI_Errhandler *errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_CREATE_ERRHANDLER(FUNCTION, ERRHANDLER, IERROR)
   	EXTERNAL FUNCTION
   	INTEGER ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_create_errhandler(win_errhandler_fn, errhandler, ierror)
   	PROCEDURE(MPI_Win_errhandler_function) :: win_errhandler_fn
   	TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


DEPRECATED TYPE NAME NOTE
-------------------------

MPI-2.2 deprecated the ``MPI_Win_errhandler_fn`` type in favor of
``MPI_Win_errhandler_function``. Open MPI supports both
names (indeed, the ``_fn`` names are typedefs to the ``_function`` names).


INPUT PARAMETER
---------------

* ``function``: User-defined error-handling procedure (function).

OUTPUT PARAMETERS
-----------------

* ``errhandler``: MPI error handler (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_create_errhandler` should be, in C, a function of type
``MPI_Win_errhandler_function``, which is defined as

.. code-block:: c

   typedef void MPI_Win_errhandler_function(MPI Win *, int *, ...);

The first argument is the window in use, the second is the error code to
be returned.

In Fortran, the user routine should be of the form:

.. code-block:: fortran

   SUBROUTINE WIN_ERRHANDLER_FUNCTION(WIN, ERROR_CODE, ...)
       INTEGER WIN, ERROR_CODE


ERRORS
------

.. include:: ./ERRORS.rst
