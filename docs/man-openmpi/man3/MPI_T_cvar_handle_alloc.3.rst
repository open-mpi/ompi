.. _mpi_t_cvar_handle_alloc:


MPI_T_cvar_handle_alloc
=======================

.. include_body

:ref:`MPI_T_cvar_handle_alloc`, :ref:`MPI_T_cvar_handle_free` - Allocate/free
control variable handles


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_cvar_handle_alloc(int cvar_index, void *obj_handle,
                               MPI_T_cvar_handle *handle, int *count)

   int MPI_T_cvar_handle_free(MPI_T_cvar_handle *handle)


DESCRIPTION
-----------

:ref:`MPI_T_cvar_handle_alloc` binds the control variable specified in
*cvar_index* to the MPI object specified in *obj_handle*. If
:ref:`MPI_T_cvar_get_info` returns MPI_T_BIND_NO_OBJECT as the binding of the
variable the *obj_handle* argument is ignored. The number of values
represented by this control variable is returned in the *count*
parameter. If the control variable represents a string then *count* will
be the maximum length of the string.

:ref:`MPI_T_cvar_handle_free` frees a handle allocated by
:ref:`MPI_T_cvar_handle_alloc` and sets the *handle* argument to
MPI_T_CVAR_HANDLE_NULL.


NOTES
-----

Open MPI does not currently support binding MPI objects to control
variables so the *obj_handle* argument is always ignored.


ERRORS
------

:ref:`MPI_T_cvar_handle_alloc` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The control variable index is invalid

* ``MPI_T_ERR_OUT_OF_HANDLES``: No more handles available

:ref:`MPI_T_cvar_handle_free` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid


.. seealso::
   * :ref:`MPI_T_cvar_get_info`
