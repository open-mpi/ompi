.. _mpi_t_cvar_write:


MPI_T_cvar_write
================

.. include_body

:ref:`MPI_T_cvar_write` |mdash| Write the value of a bound control variable


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_cvar_write(MPI_T_cvar_handle handle, const void *buf)


INPUT PARAMETERS
----------------
* ``handle``: Handle of the control variable to be written.
* ``buf``: Initial address of storage location for variable value.

DESCRIPTION
-----------

:ref:`MPI_T_cvar_write` sets the value the control variable identified
by the handle specified in *handle* from the buffer provided in
*buf*. The caller must ensure that the buffer specified in *buf* is
large enough to hold the entire value of the control variable. If the
variable has global scope, any write call must be issued on all
connected MPI processes. For more information see MPI-3 section
14.3.6.


ERRORS
------

:ref:`MPI_T_cvar_write` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid

* ``MPI_T_ERR_CVAR_SET_NOT_NOW``: Variable cannot be set at this moment

* ``MPI_T_ERR_CVAR_SET_NEVER``: Variable cannot be set until end of execution


.. seealso::
   * :ref:`MPI_T_cvar_handle_alloc`
   * :ref:`MPI_T_cvar_get_info`
