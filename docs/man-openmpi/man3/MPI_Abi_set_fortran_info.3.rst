.. _mpi_abi_set_fortran_info:


MPI_Abi_set_fortran_info
========================

.. include_body

:ref:`MPI_Abi_set_fortran_info` |mdash| Sets Fortran ABI details using supplied info object

.. The following file was automatically generated
.. include:: ./bindings/mpi_abi_set_fortran_info.rst

OUTPUT PARAMETERS
-----------------
* ``info``: Fortran ABI details info object
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Abi_set_fortran_info` allows the application to inform the implementation
of the sizes of Fortran types and whether or not optional types are supported by the Fortran
compiler. Before setting this information, the application should get this info object
using :ref:`MPI_Abi_get_fortran_info`.

NOTES
-----

The user is responsible for freeing info_used via :ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Abi_get_fortran_info`
