.. _mpi_t_pvar_write:


MPI_T_pvar_write
================

.. include_body

:ref:`MPI_T_pvar_write` |mdash| Write the value of a control variable


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_pvar_write(MPI_T_pvar_session session, MPI_T_pvar_handle handle, const void *buf)


INPUT PARAMETERS
----------------
* ``session``: Performance experiment session.
* ``handle``: Performance variable handle.
* ``buf``: Initial address of storage location for variable value.

DESCRIPTION
-----------

:ref:`MPI_T_pvar_write` attempts to set the value of the performance variable
identified by the handle specified in *handle* in the session specified
in *session*. The value to be written is specified in *buf*. The caller
must ensure that the buffer specified in *buf* is large enough to hold
the entire value of the performance variable.


ERRORS
------

:ref:`MPI_T_pvar_write` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid or not associated with the session

* ``MPI_T_ERR_INVALID_SESSION``: Session argument is not a valid session

* ``MPI_T_ERR_PVAR_NO_WRITE``: Variable cannot be written


.. seealso::
   * :ref:`MPI_T_pvar_handle_alloc`
   * :ref:`MPI_T_pvar_get_info`
   * :ref:`MPI_T_pvar_session_create`
