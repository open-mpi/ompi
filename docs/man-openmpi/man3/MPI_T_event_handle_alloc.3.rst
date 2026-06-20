.. _mpi_t_event_handle_alloc:


MPI_T_event_handle_alloc
========================

.. include_body

:ref:`MPI_T_event_handle_alloc` - Allocate event handles

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_handle_alloc.rst

INPUT PARAMETERS
----------------
* ``event_index``: Index of event type for which the registration handle is to be allocated.
* ``obj_handle``: reference to a handle of the MPI object to which this event is supposed to be bound.
* ``info``: Info object.

OUTPUT PARAMETERS
-----------------
* ``event_registration``: Event registration.


DESCRIPTION
-----------

:ref:`MPI_T_event_handle_alloc` binds the control variable specified in
*event_index* to the MPI object specified in *obj_handle*. If
:ref:`MPI_T_event_get_info` returns MPI_T_BIND_NO_OBJECT as the binding of the
variable the *obj_handle* argument is ignored.

.. note:: When the event type is bound to an object, *obj_handle* is the address
   of the variable holding that MPI object's handle, and the resulting
   registration is notified only when the event occurs for that specific object.
   For the list of Open MPI's built-in event types -- which are bound to an
   object and which are ``MPI_T_BIND_NO_OBJECT`` -- and a fuller description of
   binding, see :ref:`MPI_T_Events`.

ERRORS
------

:ref:`MPI_T_event_handle_alloc` will fail if:

.. include:: ./MPI_T_ERRORS.rst

.. seealso::
   * :ref:`MPI_T_event_get_info`
   * :ref:`MPI_T_Events`
   * :ref:`MPI_T`
