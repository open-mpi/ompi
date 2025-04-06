.. _mpi_t_event_copy:


MPI_T_event_copy
================

.. include_body

:ref:`MPI_T_event_copy` |mdash| Copy event data as a whole into a user-specified buffer.

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_copy.rst

INPUT PARAMETERS
----------------
* ``event_instance``: event-instance handle provided to the callback function.
* ``buf``: pointer to a memory location to store the event data.

DESCRIPTION
-----------

:ref:`MPI_T_event_copy` copies the event data as a whole into the user-provided buffer.
The user must assure that the buffer is of at least the size of the extent of the event
type, which can be computed from the type and displacement information returned by the
corresponding call to :ref:`MPI_T_event_get_info`.

ERRORS
------

:ref:`MPI_T_event_copy` will fail if:

.. include:: ./MPI_T_ERRORS.rst

.. seealso::
   * :ref:`MPI_T_event_read`
