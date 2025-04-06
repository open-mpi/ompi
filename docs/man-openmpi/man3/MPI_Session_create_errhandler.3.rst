.. _mpi_session_create_errhandler:

MPI_Session_create_errhandler
=============================

.. include_body

:ref:`MPI_Session_create_errhandler` |mdash| Creates an error handler that can be
attached to sessions

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_create_errhandler.rst

INPUT PARAMETER
---------------

* ``function`` : User-defined error handling procedure (function).

OUTPUT PARAMETERS
-----------------

* ``errhandler`` : MPI error handler (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_create_errhandler` creates an error handler that can be
attached to sessions. This ``function`` is identical to
:ref:`MPI_Errhandler_create`, the use of which is deprecated. In C, the
user routine should be a ``function`` of type
``MPI_Session_errhandler_function``, which is defined as

.. code-block:: c

   typedef void MPI_Session_errhandler_function(MPI_Session *, int *, ...);

The first argument is the session in use. The second is the error code
to be returned by the MPI routine that raised the error. This typedef
replaces ``MPI_Handler_function``, the use of which is deprecated. In
Fortran, the user routine should be of this form:

.. code-block:: fortran

   SUBROUTINE SESSION_ERRHANDLER_FUNCTION(SESSION, ERROR_CODE, ...)
      INTEGER SESSION, ERROR_CODE

ERRORS
------

.. include:: ./ERRORS.rst
