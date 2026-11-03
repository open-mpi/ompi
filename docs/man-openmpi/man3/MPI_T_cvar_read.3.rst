.. _mpi_t_cvar_read:


MPI_T_cvar_read
===============

.. include_body

:ref:`MPI_T_cvar_read` |mdash| Read the value of a control variable

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_cvar_read.rst

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
variable: for a string, *count* characters, where *count* is the value
returned by :ref:`MPI_T_cvar_handle_alloc`.

If a string control variable's value has grown longer than *count* - 1
characters since the handle was allocated, :ref:`MPI_T_cvar_read` stores
only its first *count* - 1 characters, followed by a null character.
Free the handle and allocate a new one to read the whole value.


ERRORS
------

:ref:`MPI_T_cvar_read` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid


.. seealso::
   * :ref:`MPI_T_cvar_handle_alloc`
   * :ref:`MPI_T_cvar_get_info`
