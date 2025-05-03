.. _mpi_t_event_get_index:


MPI_T_event_get_index
=====================

.. include_body

:ref:`MPI_T_event_get_index` |mdash| Query the index of an event type identified by a known event type name

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_get_index.rst

INPUT PARAMETERS
-----------------

* ``name``: name of the event type

OUTPUT PARAMETERS
-----------------

* ``event_index``: index of the event type


DESCRIPTION
-----------

:ref:`MPI_T_event_get_index` can be used to query for the index of an event type identified by a known
event type name.

ERRORS
------

:ref:`MPI_T_event_get_index` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface is not
  initialized

* ``MPI_T_ERR_INVALID_NAME``:  The event name is invalid
  initialized
