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

DESCRIPTION
-----------

``request`` may be a null request (``MPI_REQUEST_NULL``) or an inactive
request, in which case ``flag`` is set to true.

Calling :ref:`MPI_Parrived` on a request that does not correspond to a
partitioned receive operation is erroneous.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready_list`
   * :ref:`MPI_Pready_range`
   * :ref:`MPI_Parrived`
