.. _mpi_t_event_get_timestamp:


MPI_T_event_get_timestamp
=========================

.. include_body

:ref:`MPI_T_event_get_timestamp` |mdash|  Returns the timestamp of when the event was initially observed by the implementation

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_get_timestamp.rst

INPUT PARAMETERS
----------------
* ``event_instance``:  Event instance provided to the callback function

OUTPUT PARAMETERS
-----------------
* ``timestamp``: The timestamp when the event was observed

DESCRIPTION
-----------

:ref:`MPI_T_event_get_timestamp` returns the timestamp of when the event was initially
observed by the implementation. The ``event_instance`` argument identifies the event
instance to query.

ERRORS
------

:ref:`MPI_T_event_get_timestamp` will fail if:

.. include:: ./MPI_T_ERRORS.rst
