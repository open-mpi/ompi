.. _mpi_t_pvar_reset:


MPI_T_pvar_reset
================

.. include_body

:ref:`MPI_T_pvar_reset` |mdash| Reset the value of a performance variable


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_pvar_reset(MPI_T_pvar_session session, MPI_T_pvar_handle handle)


INPUT PARAMETERS
----------------
* ``session``: Performance experiment session.
* ``handle``: Performance variable handle or MPI_T_PVAR_ALL_HANDLES.

DESCRIPTION
-----------

:ref:`MPI_T_pvar_reset` sets the performance variable specified by the handle
in *handle* to its initial value. The special value
MPI_T_PVAR_ALL_HANDLES can be passed in *handle* to reset all read-write
handles in the session specified in *session*.


ERRORS
------

:ref:`MPI_T_pvar_reset` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid

* ``MPI_T_ERR_INVALID_SESSION``: Session argument is not a valid session

* ``MPI_T_ERR_PVAR_NO_WRITE``: Variable cannot be reset


.. seealso::
   * :ref:`MPI_T_pvar_handle_alloc`
   * :ref:`MPI_T_pvar_get_info`
   * :ref:`MPI_T_pvar_session_create`
   * :ref:`MPI_T_pvar_write`
