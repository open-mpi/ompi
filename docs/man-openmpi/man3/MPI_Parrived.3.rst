.. _mpi_parrived:


MPI_Parrived
============

.. include_body

:ref:`MPI_Parrived` |mdash| Tests for completion of a specified receive-side
partition.

.. The following file was automatically generated
.. include:: ./bindings/mpi_parrived.rst

INPUT PARAMETERS
----------------
* ``request``: Communication request (handle).
* ``partition``: The number of the partition to test for completion (integer).

OUTPUT PARAMETERS
-----------------
* ``flag``: True if partition is completed.
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready_list`
   * :ref:`MPI_Pready_range`
   * :ref:`MPI_Parrived`
