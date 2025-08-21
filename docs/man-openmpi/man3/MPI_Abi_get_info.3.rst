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

NOTES
-----

The user is responsible for freeing info_used via :ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
