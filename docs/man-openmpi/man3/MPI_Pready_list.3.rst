.. _mpi_pready_list:


MPI_Pready_list
===============

.. include_body

:ref:`MPI_Pready_list` |mdash| Indicates that a list given send-side partitions
are ready to be transferred.

.. The following file was automatically generated
.. include:: ./bindings/mpi_pready_list.rst

INPUT PARAMETERS
----------------
* ``length``: The length of the given partition array (integer).
* ``partitions``: An array of numbers of partitions to mark ready for transfer (integer).
* ``request``: Communication request (handle).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pready`
   * :ref:`MPI_Pready_range`
   * :ref:`MPI_Parrived`
