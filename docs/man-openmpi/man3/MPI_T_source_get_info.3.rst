.. _mpi_t_source_get_info:


MPI_T_source_get_info
=====================

.. include_body

:ref:`MPI_T_source_get_info` |mdash| Query information from a source

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_source_get_info.rst

INPUT PARAMETERS
----------------
* ``source_index``: Index of the source to be queried.

INPUT/OUTPUT PARAMETERS
-----------------------
* ``name_len``: Length of the string and/or buffer for name.
* ``desc_len``: Length of the string and/or buffer for desc.

OUTPUT PARAMETERS
-----------------
* ``name``: Buffer to return the string containing the name of the source.
* ``desc``: Buffer to return the string containing the description of the source.
* ``ordering``: Flag indicating chronological ordering guarantees given by the source.
* ``ticks_per_second``: The number of ticks per second for the timer of this source.
* ``max_ticks``: The maximum count of ticks reported by this source before overflow occurs.
* ``info``: Info argument.

DESCRIPTION
-----------

:ref:`MPI_T_source_get_info` can be used to query information from a
source.


ERRORS
------

:ref:`MPI_T_source_get_info` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The source index is invalid
