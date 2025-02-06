.. _mpi_t_event_callback_set_info:


MPI_T_event_callback_set_info
=============================

.. include_body

:ref:`MPI_T_event_callback_set_info` |mdash| Updates the hints of the callback function registered for the callback safety level specified by cb_safety of the event-registration handle associated with event_registration.

SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_event_callback_set_info(MPI_T_event_registration event_registration,
                                     MPI_T_cb_safety cb_safety, MPI_Info info)


INPUT PARAMETERS
----------------
* ``event_registration``: Event registration
* ``cb_safety``: maximum callback safety level
* ``info``: Info argument

DESCRIPTION
-----------

:ref:`MPI_T_event_callback_set_info` Updates the hints of the callback function registered for the callback safety level specified by `cb_safety` of the event-registration handle associated with event_registration.


ERRORS
------

:ref:`MPI_T_event_callback_set_info` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid

* ``MPI_T_ERR_INVALID_HANDLE``: Invalid use of the interface or bad parameter values(s).

* ``MPI_T_ERR_OTHER``: Other error
