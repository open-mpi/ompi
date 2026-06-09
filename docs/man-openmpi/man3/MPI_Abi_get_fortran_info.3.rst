.. _mpi_abi_get_fortran_info:


MPI_Abi_get_fortran_info
========================

.. include_body

:ref:`MPI_Abi_get_fortran_info` |mdash| Retrieves Fortran ABI details info object

.. The following file was automatically generated
.. include:: ./bindings/mpi_abi_get_fortran_info.rst

OUTPUT PARAMETERS
-----------------
* ``info``: New info object returned with Fortran ABI details, or
  ``MPI_INFO_NULL`` if no Fortran ABI details are available.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Abi_get_fortran_info` returns a new info object providing
additional information related to the Fortran ABI.

When Open MPI is built with Fortran bindings, the returned info object
describes Open MPI's configured Fortran support.  It may contain the
following keys:

* ``mpi_logical_size``: size, in bytes, of default Fortran ``LOGICAL``.
* ``mpi_integer_size``: size, in bytes, of default Fortran ``INTEGER``.
* ``mpi_real_size``: size, in bytes, of default Fortran ``REAL``.
* ``mpi_double_precision_size``: size, in bytes, of Fortran
  ``DOUBLE PRECISION``.
* ``mpi_logical1_supported``, ``mpi_logical2_supported``,
  ``mpi_logical4_supported``, ``mpi_logical8_supported``, and
  ``mpi_logical16_supported``: whether each optional Fortran logical
  datatype is supported.
* ``mpi_integer1_supported``, ``mpi_integer2_supported``,
  ``mpi_integer4_supported``, ``mpi_integer8_supported``, and
  ``mpi_integer16_supported``: whether each optional Fortran integer
  datatype is supported.
* ``mpi_real2_supported``, ``mpi_real4_supported``,
  ``mpi_real8_supported``, and ``mpi_real16_supported``: whether each
  optional Fortran real datatype is supported.
* ``mpi_complex4_supported``, ``mpi_complex8_supported``,
  ``mpi_complex16_supported``, and ``mpi_complex32_supported``: whether
  each optional Fortran complex datatype is supported.
* ``mpi_double_complex_supported``: whether ``MPI_DOUBLE_COMPLEX`` is
  supported.

The ``*_supported`` values are strings with values ``true`` or
``false``.

When Open MPI is built without Fortran bindings, this routine returns a
duplicate of the info object previously supplied to
:ref:`MPI_Abi_set_fortran_info`, if any.  If no such object has been
supplied, the returned handle is ``MPI_INFO_NULL``.

NOTES
-----

The user is responsible for freeing ``info`` via :ref:`MPI_Info_free`
when ``info`` is not ``MPI_INFO_NULL``.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Abi_set_fortran_info`
