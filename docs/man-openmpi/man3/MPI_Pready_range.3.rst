.. _mpi_pready_range:


MPI_Pready_range
================

.. include_body

:ref:`MPI_Pready_range` |mdash| Indicates that a given range os send-side
partitions are ready to be transferred.

.. The following file was automatically generated
.. include:: ./bindings/mpi_pready_range.rst

INPUT PARAMETERS
----------------
* ``partition_low``: The lowest of the range of partitions to mark ready for transfer (integer).
* ``partition_high``: The highest of the range of partitions to mark ready for transfer (integer).
* ``request``: Communication request (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready`
   * :ref:`MPI_Pready_list`
   * :ref:`MPI_Parrived`
