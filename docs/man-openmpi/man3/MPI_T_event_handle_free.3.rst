.. _mpi_t_event_handle_free:


MPI_T_event_handle_free
=======================

.. include_body

:ref:`MPI_T_event_handle_free` - Free event handles

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_handle_free.rst

INPUT PARAMETERS
----------------
* ``user_data``: Pointer to a user-controlled buffer.
* ``free_cb_function``: Pointer to user-defined callback function.

INPUT/OUTPUT PARAMETERS
-----------------------
* ``event_registration``: Event registration.


DESCRIPTION
-----------

:ref:`MPI_T_event_handle_free` frees a handle allocated by
:ref:`MPI_T_event_handle_alloc`.

ERRORS
------

:ref:`MPI_T_event_handle_free` will fail if:

.. include:: ./MPI_T_ERRORS.rst

.. seealso::
   * :ref:`MPI_T_event_get_info`
