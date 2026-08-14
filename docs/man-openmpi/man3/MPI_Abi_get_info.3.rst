.. _mpi_abi_get_info:


MPI_Abi_get_info
================

.. include_body

:ref:`MPI_Abi_get_info` |mdash| Retrieves info hints about the ABI implementation

.. The following file was automatically generated
.. include:: ./bindings/mpi_abi_get_info.rst

OUTPUT PARAMETERS
-----------------
* ``info``: New info object returned with ABI details.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Abi_get_info` returns a new info object providing
additional information related to the ABI.

Open MPI currently returns the following info keys:

* ``mpi_aint_size``: size, in bytes, of ``MPI_Aint``.
* ``mpi_count_size``: size, in bytes, of ``MPI_Count``.
* ``mpi_offset_size``: size, in bytes, of ``MPI_Offset``.

NOTES
-----

The user is responsible for freeing ``info`` via :ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
