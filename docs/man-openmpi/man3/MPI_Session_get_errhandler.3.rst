.. _mpi_session_get_errhandler:


MPI_Session_get_errhandler
==========================

.. include_body

:ref:`MPI_Session_get_errhandler` - Retrieves error handler associated with a
session.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_get_errhandler(MPI_Session session,
   	MPI_Errhandler *errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SESSION_GET_ERRHANDLER(SESSION, ERRHANDLER, IERROR)
   	INTEGER	SESSION, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Session_get_errhandler(session, errhandler, ierror)
   	TYPE(MPI_Session), INTENT(IN) :: session
   	TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``session``: Session (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: New error handler for session (handle).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_get_errhandler` retrieves the error handler currently associated
with a session. 


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Session_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.

See the MPI man page for a full list of MPI error codes.
