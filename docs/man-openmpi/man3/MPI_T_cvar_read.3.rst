.. _mpi_t_cvar_read:


MPI_T_cvar_read
===============

.. include_body

:ref:`MPI_T_cvar_read` |mdash| Read the value of a control variable


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_cvar_read(MPI_T_cvar_handle handle, const void *buf)


INPUT PARAMETERS
----------------
* ``handle``: Handle of the control variable to be read.
* ``buf``: Initial address of storage location for variable value.

DESCRIPTION
-----------

:ref:`MPI_T_cvar_read` reads the value of the control variable identified by
the handle specified in *handle* and stores the value in the buffer
pointed to by *buf*. The caller must ensure that the buffer pointed to
by *buf* is large enough to hold the entire value of the control
variable.


ERRORS
------

:ref:`MPI_T_cvar_read` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid


.. seealso::
   * :ref:`MPI_T_cvar_handle_alloc`
   * :ref:`MPI_T_cvar_get_info`
