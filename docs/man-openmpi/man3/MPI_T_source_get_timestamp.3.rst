.. _mpi_t_source_get_timestamp:


MPI_T_source_get_timestamp
========================== 

.. include_body

:ref:`MPI_T_source_get_timestamp` |mdash| Returns a current timestamp from the source identified by the source_index argument 


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_T_source_get_timestamp(int source_index, MPI_Count *timestamp)


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
