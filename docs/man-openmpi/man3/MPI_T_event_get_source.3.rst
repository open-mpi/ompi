.. _mpi_t_event_get_source:


MPI_T_event_get_source
=======================

.. include_body

:ref:`MPI_T_event_get_source` |mdash|  Returns the index of the source of the event instance

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_get_source.rst

INPUT PARAMETERS
----------------
* ``event_instance``:  Event instance provided to the callback function

OUTPUT PARAMETERS
-----------------
* ``source_index``: Index identifying the source

DESCRIPTION
-----------

:ref:`MPI_T_event_get_source` returns the index of the source of ``event_instance``.

ERRORS
------

:ref:`MPI_T_event_get_source` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The handle is invalid

* ``MPI_T_ERR_INVALID``: Invalid use of the interface or bad parameter values(s)

* ``MPI_ERR_OTHER``: Other error
