.. _mpi_abi_get_fortran_info:


MPI_Abi_get_fortran_info
========================

.. include_body

:ref:`MPI_Abi_get_fortran_info` |mdash| Retrieves Fortran ABI details info object

.. The following file was automatically generated
.. include:: ./bindings/mpi_abi_get_fortran_info.rst

OUTPUT PARAMETERS
-----------------
* ``info``: New info object returned with Fortran ABI details.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Abi_get_fortran_info` returns a new info object providing
additional information related to the Fortran ABI.

NOTES
-----

The user is responsible for freeing info_used via :ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Abi_set_fortran_info`
