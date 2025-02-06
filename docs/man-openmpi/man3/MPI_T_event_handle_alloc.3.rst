.. _mpi_t_event_handle_alloc:


MPI_T_event_handle_alloc
========================

.. include_body

:ref:`MPI_T_event_handle_alloc`, :ref:`MPI_T_event_handle_free` - Allocate/free
event handles


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_event_handle_alloc(int event_index, void *obj_handle,
                                MPI_T_event_registration *event_registration)

   int MPI_T_event_handle_free(MPI_T_event_registration event_registration,
                               void *user_data, MPI_T_event_free_cb_function free_cb_function)

DESCRIPTION
-----------

:ref:`MPI_T_event_handle_alloc` binds the control variable specified in
*event_index* to the MPI object specified in *obj_handle*. If
:ref:`MPI_T_event_get_info` returns MPI_T_BIND_NO_OBJECT as the binding of the
variable the *obj_handle* argument is ignored.

:ref:`MPI_T_event_handle_free` frees a handle allocated by
:ref:`MPI_T_event_handle_alloc`.

ERRORS
------

:ref:`MPI_T_event_handle_alloc` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The supplied event index is invalid

* ``MPI_T_ERR_INVALID``: The supplied input parameter is invalid

.. seealso::
   * :ref:`MPI_T_event_get_info`
