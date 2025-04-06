.. _mpi_win_create_errhandler:


MPI_Win_create_errhandler
=========================

.. include_body

:ref:`MPI_Win_create_errhandler` |mdash| Creates an error handler for a window.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_create_errhandler.rst

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
