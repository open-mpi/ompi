.. _mpi_t_event_callback_get_info:


MPI_T_event_callback_get_info
=============================

.. include_body

:ref:`MPI_T_event_callback_get_info` |mdash| Returns a new info object containing the hints of the callback function registered for the callback safety level specified by cb_safety of the event-registration handle associated with event_registration.

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_callback_get_info.rst

INPUT PARAMETERS
----------------
* ``event_registration``: Event registration
* ``cb_safety``: maximum callback safety level

OUTPUT PARAMETERS
-----------------

* ``info_used``: Info argument

DESCRIPTION
-----------

:ref:`MPI_T_event_callback_get_info` returns a new info object containing the
hints of the callback function registered for the callback safety level specified by ``cb_safety`` of
the event-registration handle associated with ``event_registration``.


ERRORS
------

:ref:`MPI_T_event_callback_get_info` will fail if:

.. include:: ./MPI_T_ERRORS.rst
