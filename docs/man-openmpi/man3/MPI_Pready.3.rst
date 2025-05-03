.. _mpi_pready:


MPI_Pready
==========

.. include_body

:ref:`MPI_Pready` |mdash| Indicates that a given send-side partition is ready to
be transferred.

.. The following file was automatically generated
.. include:: ./bindings/mpi_pready.rst

INPUT PARAMETERS
----------------
* ``partition``: The number of the partition to mark ready for transfer (integer).
* ``request``: Communication request (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready_list`
   * :ref:`MPI_Pready_range`
   * :ref:`MPI_Parrived`
