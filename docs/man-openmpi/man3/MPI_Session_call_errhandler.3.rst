.. _mpi_session_call_errhandler:

MPI_Session_call_errhandler
===========================

.. include_body

:ref:`MPI_Session_call_errhandler` |mdash| Passes the supplied error code to the error
handler assigned to a session

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_call_errhandler(MPI_Session session, int errorcode)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_CALL_ERRHANDLER(SESSION, ERRORCODE, IERROR)
       INTEGER SESSION, ERRORCODE, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Session_call_errhandler(session, errorcode, ierror)
       TYPE(MPI_Session), INTENT(IN) :: session
       INTEGER, INTENT(IN) :: errorcode
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
---------------

* ``session`` : session with error handler (handle).
* ``errorcode`` : error code (integer).

OUTPUT PARAMETERS
-----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This function invokes the error handler assigned to the session
session with the supplied error code errorcode. If the error handler was
successfully called, the process is not aborted, and the error handler
returns, this function returns MPI_SUCCESS.

NOTES
-----

Users should note that the default error handler is
MPI_ERRORS_ARE_FATAL. Thus, calling this function will abort the
processes in session if the default error handler has not been changed.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_create_errhandler`
