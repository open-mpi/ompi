.. _mpi_t_pvar_handle_alloc:


MPI_T_pvar_handle_alloc
=======================

.. include_body

:ref:`MPI_T_pvar_handle_alloc`, :ref:`MPI_T_pvar_handle_free` - Allocate/free
MPI performance variable handles


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_pvar_handle_alloc(int session, int pvar_index, void *obj_handle,
                               MPI_T_pvar_handle *handle, int *count)

   int MPI_T_pvar_handle_free(int session, MPI_T_pvar_handle *handle)


DESCRIPTION
-----------

:ref:`MPI_T_pvar_handle_alloc` binds the performance variable specified in
*pvar_index* to the MPI object specified in *obj_handle* in the session
identified by the parameter *session*. The object is passed in the
argument *obj_handle* as an address to a local variable that stores the
object’s handle. If :ref:`MPI_T_pvar_get_info` returns MPI_T_BIND_NO_OBJECT as
the binding for the variable the *obj_handle* argument is ignored. The
handle allocated to reference the variable is returned in the argument
*handle*. Upon successful return, *count* contains the number of
elements (of the datatype returned by a previous :ref:`MPI_T_PVAR_GET_INFO`
call) used to represent this variable.

The value of *pvar_index* should be in the range 0 to *num_pvar - 1*,
where *num_pvar* is the number of available performance variables as
determined from a prior call to :ref:`MPI_T_PVAR_GET_NUM`. The type of the
MPI object it references must be consistent with the type returned in
the bind argument in a prior call to :ref:`MPI_T_PVAR_GET_INFO`.

:ref:`MPI_T_pvar_handle_free` frees a handle allocated by
:ref:`MPI_T_pvar_handle_alloc` and sets the *handle* argument to
MPI_T_PVAR_HANDLE_NULL.


ERRORS
------

:ref:`MPI_T_pvar_handle_alloc` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The performance variable index is invalid

* ``MPI_T_ERR_OUT_OF_HANDLES``: No more handles available

:ref:`MPI_T_pvar_handle_free` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid or the handle
  argument passed in is not associated with the session argument


.. seealso::
   * :ref:`MPI_T_pvar_get_info`
   * :ref:`MPI_T_pvar_get_num`
