.. _mpi_t_event_set_dropped_handler:


MPI_T_event_set_dropped_handler
===============================

.. include_body

:ref:`MPI_T_event_set_dropped_handler` |mdash|  Registers a function to be called when event information is dropped for the registration handle specified in event_registration


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_event_set_dropped_handler(MPI_T_event_registration event_registration,
                                       MPI_T_event_dropped_cb_function dropped_cb_function) 


INPUT PARAMETERS
----------------
* ``event_registration``: Event registration
* ``dropped_cb_function``:  pointer to user-defined callback function

DESCRIPTION
-----------

:ref:`MPI_T_event_set_dropped_handler` registers the function
``dropped_cb_function`` to be called by the MPI implementation when event information is
dropped for the registration handle specified in ``event_registration``.


ERRORS
------

:ref:`MPI_T_event_set_dropped_handler` will fail if:

.. include:: ./MPI_T_ERRORS.rst
