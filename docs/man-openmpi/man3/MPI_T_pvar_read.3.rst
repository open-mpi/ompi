.. _mpi_t_pvar_read:


MPI_T_pvar_read
===============

.. include_body

:ref:`MPI_T_pvar_read` |mdash| Read the value of a performance variable


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_pvar_read(MPI_T_pvar_session session, MPI_T_pvar_handle handle, const void *buf)


INPUT PARAMETERS
----------------
* ``session``: Performance experiment session.
* ``handle``: Performance variable handle.
* ``buf``: Initial address of storage location for variable value.

DESCRIPTION
-----------

:ref:`MPI_T_pvar_read` queries the value of a performance variable identified
by the handle specified in *handle* in the session specified in
*session*. The result is stored in the buffer pointed to by *buf*. The
caller must ensure that the buffer pointed to by *buf* is large enough
to hold the entire value of the performance variable.


ERRORS
------

:ref:`MPI_T_pvar_read` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid or not associated with the session

* ``MPI_T_ERR_INVALID_SESSION``: Session argument is not a valid session


.. seealso::
   * :ref:`MPI_T_pvar_handle_alloc`
   * :ref:`MPI_T_pvar_get_info`
   * :ref:`MPI_T_pvar_session_create`
