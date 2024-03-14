.. _mpi_t_pvar_session_create:


MPI_T_pvar_session_create
=========================

.. include_body

:ref:`MPI_T_pvar_session_create`, :ref:`MPI_T_pvar_session_free` - Create/free
performance variable session


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_pvar_session_create(MPI_T_pvar_session *session)

   int MPI_T_pvar_session_free(MPI_T_pvar_session *session)


DESCRIPTION
-----------

:ref:`MPI_T_pvar_session_create` creates a session for accessing performance
variables. The new session is returned in the *session* parameter.

:ref:`MPI_T_pvar_session_free` releases a session allocated by
:ref:`MPI_T_pvar_session_create` and sets the *session* parameter to
MPI_T_PVAR_SESSION_NULL.


ERRORS
------

:ref:`MPI_T_pvar_session_create` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_MEMORY``: Out of memory

* ``MPI_T_ERR_OUT_OF_SESSIONS``: No more sessions available

:ref:`MPI_T_pvar_session_free` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_SESSION``: The session parameter is not a valid session
