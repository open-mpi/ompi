.. _mpi_t_event_get_timestamp:


MPI_T_event_get_timestamp
=========================

.. include_body

:ref:`MPI_T_event_get_timestamp` |mdash|  Returns the timestamp of when the event was initially observed by the implementation


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_event_get_timestamp(MPI_T_event_instance event_instance, MPI_Count *event_timestamp)


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

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The handle is invalid

* ``MPI_T_ERR_INVALID``: Invalid use of the interface or bad parameter values(s)
