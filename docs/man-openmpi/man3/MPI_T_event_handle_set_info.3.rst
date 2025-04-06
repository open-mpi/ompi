.. _mpi_t_event_handle_set_info:


MPI_T_event_handle_set_info
===========================

.. include_body

:ref:`MPI_T_event_handle_set_info` |mdash| Updates the hints of the event-registration handle associated with event_registration.

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_handle_set_info.rst

INPUT PARAMETERS
----------------
* ``event_registration``: Event registration to be queried.
* ``info``: Info argument.

DESCRIPTION
-----------

:ref:`MPI_T_event_handle_set_info` updates the hints of the event-registration handle
associated with event_registration using the hints provided in info.


ERRORS
------

:ref:`MPI_T_source_get_info` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The event registration is invalid

* ``MPI_T_ERR_INFO``: Invalid info
