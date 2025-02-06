.. _mpi_t_event_read:


MPI_T_event_read
================

.. include_body

:ref:`MPI_T_event_read` |mdash| Read the value of a event instance.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_event_read(MPI_T_event_instance event_instance, int elem_index, void *buffer)


INPUT PARAMETERS
----------------
* ``event_instance``: event-instance handle provided to the callback function.
* ``elem_index``: index into the array of datatypes of the item to be queried.
* ``buf``: pointer to a memory location to store the item data

DESCRIPTION
-----------

:ref:`MPI_T_event_read` allows users to copy one element of the event data to a userspecified
buffer at a time.

ERRORS
------

:ref:`MPI_T_event_read` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_HANDLE``: The handle is invalid

* ``MPI_T_ERR_INVALID``: Invalid use of the interface or bad parameter values(s).


.. seealso::
   * :ref:`MPI_T_event_copy`
