.. _mpi_t_event_get_info:


MPI_T_event_get_info
====================

.. include_body

:ref:`MPI_T_event_get_info` |mdash|  Returns additional information about a specific event type

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_event_get_info.rst

INPUT PARAMETERS
----------------
* ``event_index``: Index of the event to be queried.

INPUT/OUTPUT PARAMETERS
-----------------------
* ``name_len``: Length of the string and/or buffer for name.
* ``desc_len``: Length of the string and/or buffer for desc.
* ``num_elements``: length of array of datatypes and displacements.

OUTPUT PARAMETERS
-----------------
* ``name``: Buffer to return the string containing the name of the event type.
* ``verbosity``: Verbosity level of this event type
* ``array_of_datatypes``:  Array of MPI basic datatypes used to encode the event data
* ``array_of_displacements``:  Array of byte displacements of the elements in the event buffer
* ``enumtype``:  Optional descriptor for enumeration information
* ``info``: Optional info argument.
* ``desc``: Buffer to return the string containing the description of the event type.
* ``bind``:  Type of MPI object to which an event of this type must be bound

DESCRIPTION
-----------

:ref:`MPI_T_event_get_info` can be used to query information from for a specific event type.


ERRORS
------

:ref:`MPI_T_event_get_info` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The source index is invalid

* ``MPI_T_ERR_INVALID``: Invalid use of the interface or bad parameter values(s).

* ``MPI_ERR_OTHER``: Other error
