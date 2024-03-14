.. _mpi_t_pvar_readreset:


MPI_T_pvar_readreset
====================

.. include_body

:ref:`MPI_T_pvar_readreset` |mdash| Atomically read and reset the value of a
performance variable


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_pvar_readreset(MPI_T_pvar_session session, MPI_T_pvar_handle handle, const void *buf)


INPUT PARAMETERS
----------------
* ``session``: Performance experiment session.
* ``handle``: Performance variable handle.
* ``buf``: Initial address of storage location for variable value.

DESCRIPTION
-----------

:ref:`MPI_T_pvar_readreset` atomically queries and resets the value of a
performance variable bound to the handle specified by *handle* in the
session specified by *session*. The result is stored in the buffer
pointed to by *buf*. This function can only be used with performance
variables that are atomic and not readonly. The caller must ensure that
the buffer pointed to by *buf* is large enough to hold the entire value
of the performance variable.


ERRORS
------

:ref:`MPI_T_pvar_readreset` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid or not associated with the session

* ``MPI_T_ERR_INVALID_SESSION``: Session argument is not a valid session

* ``MPI_T_ERR_PVAR_NO_ATOMIC``: Variable cannot be read and written atomically

* ``MPI_T_ERR_PVAR_NO_WRITE``: Variable cannot be reset


.. seealso::
   * :ref:`MPI_T_pvar_handle_alloc`
   * :ref:`MPI_T_pvar_get_info`
   * :ref:`MPI_T_pvar_session_create`
   * :ref:`MPI_T_pvar_read`
   * :ref:`MPI_T_pvar_reset`
