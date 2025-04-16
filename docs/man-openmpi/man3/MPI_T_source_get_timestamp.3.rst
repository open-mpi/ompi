.. _mpi_t_source_get_timestamp:


MPI_T_source_get_timestamp
==========================

.. include_body

:ref:`MPI_T_source_get_timestamp` |mdash| Returns a current timestamp from the source identified by the source_index argument

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_source_get_timestamp.rst

INPUT PARAMETERS
----------------
* ``source_index``: Index of the source to be queried.

OUTPUT PARAMETERS
-----------------
* ``timestamp``: The current timestamp from specified source.

DESCRIPTION
-----------

:ref:`MPI_T_source_get_timestamp` returns the current timestamp from the specificed source.


ERRORS
------

:ref:`MPI_T_source_get_timestamp` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface not initialized

* ``MPI_T_ERR_INVALID_INDEX``: The source index is invalid
