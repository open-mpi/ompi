.. _mpi_abi_set_fortran_info:


MPI_Abi_set_fortran_info
========================

.. include_body

:ref:`MPI_Abi_set_fortran_info` |mdash| Sets Fortran ABI details using supplied info object

.. The following file was automatically generated
.. include:: ./bindings/mpi_abi_set_fortran_info.rst

INPUT PARAMETERS
----------------
* ``info``: Fortran ABI details info object

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Abi_set_fortran_info` allows the application to inform the implementation
of the sizes of Fortran types and whether or not optional types are supported by the Fortran
compiler. Before setting this information, the application should get this info object
using :ref:`MPI_Abi_get_fortran_info`.

Open MPI uses its configured Fortran support when it is built with
Fortran bindings.  In that case, this routine returns ``MPI_ERR_ABI``
and does not replace Open MPI's configured Fortran ABI information.

When Open MPI is built without Fortran bindings, this routine stores a
duplicate of ``info`` for later retrieval via
:ref:`MPI_Abi_get_fortran_info`.  This routine may be called at most
once; a subsequent call returns ``MPI_ERR_ABI``.

NOTES
-----

The application retains ownership of ``info`` and is responsible for freeing it via :ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Abi_get_fortran_info`
