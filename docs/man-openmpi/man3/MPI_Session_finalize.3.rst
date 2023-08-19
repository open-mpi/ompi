.. _mpi_session_finalize:

MPI_Session_finalize
====================

.. include_body

:ref:`MPI_Session_finalize` - releases all MPI state associated with a session

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Session_finalize(MPI_Session *session)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_SESSION_FINALIZE(SESSION, IERROR)
       INTEGER SESSION, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Session_finalize(session, ierror)
       TYPE(MPI_Session), INTENT(IN) :: session
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``session`` : session to be finalized (handle)

OUTPUT PARAMETERS
-----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_finalize` releases all MPI state associated with the supplied
session. Every instantiated session must be finalized using
:ref:`MPI_Session_finalize`. The handle session is set to MPI_SESSION_NULL by
the call.

NOTES
-----

Before an MPI process invokes :ref:`MPI_Session_finalize`, the process must
perform all MPI calls needed to complete its involvement in MPI
communications: it must locally complete all MPI operations that it
initiated and it must execute matching calls needed to complete MPI
communications initiated by other processes. The call to
:ref:`MPI_Session_finalize` does not free objects created by MPI calls; these
objects are freed using MPI_XXX_FREE calls. :ref:`MPI_Session_finalize` may be
synchronizing on any or all of the groups associated with communicators,
windows, or files derived from the session and not disconnected, freed,
or closed, respectively, before the call to :ref:`MPI_Session_finalize`
procedure. :ref:`MPI_Session_finalize` behaves as if all such synchronizations
occur concurrently. As :ref:`MPI_Comm_free` may mark a communicator for freeing
later, :ref:`MPI_Session_finalize` may be synchronizing on the group associated
with a communicator that is only freed (with MPI_Comm_free) rather than
disconnected (with MPI_Comm_disconnect).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Session_init`
