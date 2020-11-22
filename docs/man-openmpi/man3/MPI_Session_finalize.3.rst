.. _mpi_session_finalize:

MPI_Session_finalize
====================

.. include_body

:ref:`MPI_Session_finalize` - releases all MPI state associated with a session

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Session_finalize(MPI_Session *session)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_FINALIZE(SESSION, IERROR)
       INTEGER SESSION, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Session_finalize(session, ierror)
       TYPE(MPI_Session), INTENT(IN) :: session
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  session : session to be finalized (handle)

Output Parameters
-----------------

-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Session_finalize` releases all MPI state associated with the supplied
session. Every instantiated session must be finalized using
:ref:`MPI_Session_finalize`. The handle session is set to MPI_SESSION_NULL by
the call.

Notes
-----

Before an MPI process invokes :ref:`MPI_Session_finalize`, the process must
perform all MPI calls needed to complete its involvement in MPI
communications: it must locally complete all MPI operations that it
initiated and it must execute matching calls needed to complete MPI
communications initiated by other processes. The call to
:ref:`MPI_Session_finalize` does not free objects created by MPI calls; these
objects are freed using MPI_XXX_FREE calls. :ref:`MPI_Session_finalize` may be
synchronizing on any or all of the groups associated with communicators,
windows, or  les derived from the session and not disconnected, freed,
or closed, respectively, before the call to :ref:`MPI_Session_finalize`
procedure. :ref:`MPI_Session_finalize` behaves as if all such synchronizations
occur concurrently. As :ref:`MPI_Comm_free` may mark a communicator for freeing
later, :ref:`MPI_Session_finalize` may be synchronizing on the group associated
with a communicator that is only freed (with MPI_Comm_free) rather than
disconnected (with MPI_Comm_disconnect).

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with
MPI_Session_set_errhandler; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned. Note
that MPI does not guarantee that an MPI program can continue past an
error.


.. seealso:: :ref:`MPI_Session_init`
