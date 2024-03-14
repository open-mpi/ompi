.. _mpi_t_pvar_start:


MPI_T_pvar_start
================

.. include_body

:ref:`MPI_T_pvar_start`, :ref:`MPI_T_pvar_stop` - Start/stop a performance
variable


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_pvar_start(MPI_T_pvar_session session, MPI_T_pvar_handle handle)

   int MPI_T_pvar_stop(MPI_T_pvar_session session, MPI_T_pvar_handle handle)


INPUT PARAMETERS
----------------
* ``session``: Performance experiment session.
* ``handle``: Performance variable handle.

DESCRIPTION
-----------

:ref:`MPI_T_pvar_start` starts the performance variable with the handle
specified in *handle*. The special value MPI_T_PVAR_ALL_HANDLES can be
passed in *handle* to start all non-continuous handles in the session
specified in *session*.

:ref:`MPI_T_pvar_stop` stops the performance variable with the handle specified
in *handle*. The special value MPI_T_PVAR_ALL_HANDLES can be passed in
*handle* to stop all non-continuous handles in the session specified in
*session*.

Continuous performance variables can neither be started nor stopped.


ERRORS
------

:ref:`MPI_T_pvar_start` and MPI_T_pvar_stop() will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_SESSION``: Session parameter is not a valid session

* ``MPI_T_ERR_INVALID_HANDLE``: Invalid handle or handle not associated with the session

* ``MPI_T_ERR_PVAR_NO_STARTSTOP``: The variable cannot be started or stopped


.. seealso::
   * :ref:`MPI_T_pvar_get_info`
