.. _mpi_session_finalize:

MPI_Session_finalize
====================

.. include_body

:ref:`MPI_Session_finalize` |mdash| releases all MPI state associated with a session

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_finalize.rst

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
:ref:`MPI_Session_finalize`. The handle session is set to ``MPI_SESSION_NULL`` by
the call.

Multiple sessions can be created and destroyed during the lifetime of
an MPI process.  This is different than MPI world model, which can be
initialized at most exactly once (and then subsequently finalized)
during the lifetime of an MPI process.


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

.. seealso::
   * :ref:`MPI_Init`
   * :ref:`MPI_Initialized`
   * :ref:`MPI_Init_thread`
   * :ref:`MPI_Finalize`
   * :ref:`MPI_Finalized`
   * :ref:`MPI_Session_init`
