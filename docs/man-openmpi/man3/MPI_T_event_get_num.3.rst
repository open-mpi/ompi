.. _mpi_t_event_get_num:


MPI_T_event_get_num
===================

.. include_body

:ref:`MPI_T_event_get_num` |mdash| Query the number of MPI_T event types

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_get_num.rst

OUTPUT PARAMETERS
-----------------

* ``num_events``: Returns number of event types


DESCRIPTION
-----------

:ref:`MPI_T_event_get_num` can be used to query the current number of MPI_T event types.

The number of available event types can be queried with a call to
:ref:`MPI_T_event_get_num`. An MPI implementation is allowed to increase the number of event types during the
execution of an MPI process. However, MPI implementations are not allowed to change the
index of an event type or to delete an event type once it has been made visible to the user.

.. note:: Open MPI will currently return that there are 0 event types.


ERRORS
------

:ref:`MPI_T_event_get_num` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface is not
  initialized
