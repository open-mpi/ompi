.. _mpi_session_create_errhandler:

MPI_Session_create_errhandler
=============================

.. include_body

:ref:`MPI_Session_create_errhandler` |mdash| Creates an error handler that can be
attached to sessions

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_create_errhandler(MPI_Session_errhandler_function *function,
       MPI_Errhandler *errhandler)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_CREATE_ERRHANDLER(FUNCTION, ERRHANDLER, IERROR)
       EXTERNAL    FUNCTION
       INTEGER ERRHANDLER, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Session_create_errhandler(session_errhandler_fn, errhandler, ierror)
       PROCEDURE(MPI_Session_errhandler_function) :: session_errhandler_fn
       TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
