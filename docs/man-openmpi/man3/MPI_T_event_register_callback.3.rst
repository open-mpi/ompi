.. _mpi_t_event_register_callback:


MPI_T_event_register_callback
=============================

.. include_body

:ref:`MPI_T_event_register_callback` |mdash| Associates a user-defined function with an allocated event-registration handle


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_event_register_callback(MPI_T_event_registration event_registration,
                                     MPI_T_cb_safety cb_safety, MPI_Info info,
                                     void *user_data, MPI_T_event_cb_funciton event_cb_function) 


INPUT PARAMETERS
----------------
* ``event_registration``: Event registration
* ``cb_safety``: maximum callback safety level
* ``info``: Info argument
* ``user_data``: pointer to a user-controlled buffer
* ``event_cb_function``:  pointer to user-defined callback function

DESCRIPTION
-----------

:ref:`MPI_T_event_register_callback` associates a user-defined function pointed to
by ``event_cb_function`` with an allocated event-registration handle. The maximum callback
safety level supported by the callback function is passed in the argument ``cb_safety``.


ERRORS
------

:ref:`MPI_T_event_register_callback` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid

* ``MPI_T_ERR_INFO``: Invalid info

* ``MPI_T_ERR_OTHER``: Other error
