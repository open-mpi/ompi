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

When Open MPI is built without Fortran bindings (``--disable-mpi-fortran``),
the implementation does not know the Fortran compiler's type properties,
so :ref:`MPI_Abi_get_fortran_info` returns ``MPI_INFO_NULL`` and the
application must supply them through ``info``.  This routine then
configures the Fortran predefined datatypes from the following keys so
that, for example, :ref:`MPI_Type_size` reports the application-specified
size instead of ``MPI_UNDEFINED``:

* ``mpi_logical_size``: size in bytes of Fortran default ``LOGICAL``
* ``mpi_integer_size``: size in bytes of Fortran default ``INTEGER``
* ``mpi_real_size``: size in bytes of Fortran default ``REAL``; this
  also configures ``MPI_COMPLEX``, which is a pair of default ``REAL``
  values (``2 * mpi_real_size`` bytes)
* ``mpi_double_precision_size``: size in bytes of Fortran ``DOUBLE
  PRECISION``; this also configures ``MPI_DOUBLE_COMPLEX``, which is a
  pair of ``DOUBLE PRECISION`` values (``2 * mpi_double_precision_size``
  bytes)
* ``mpi_<type><n>_supported`` (boolean): whether the Fortran compiler
  supports the optional fixed-size type ``MPI_<TYPE><N>`` (for example
  ``mpi_integer8_supported`` for ``MPI_INTEGER8``)

When a ``mpi_<type><n>_supported`` key is ``true``, Open MPI makes the
corresponding predefined datatype available with its inherent
``<n>``-byte size *when this build can represent a type of that size*.
A build that has no underlying type of the requested size (for example
``MPI_REAL16`` on a platform whose widest floating-point type is 8
bytes) cannot provide that datatype; the key is accepted but the
datatype remains unavailable.  The keys therefore describe the Fortran
compiler's capabilities, not a guarantee of availability: as the
standard specifies, an application determines whether a given optional
datatype is actually available by calling :ref:`MPI_Type_size` and
checking for ``MPI_UNDEFINED``.

This routine may be called at most once; a subsequent call returns
``MPI_ERR_ABI``.  A duplicate of ``info`` is also stored for later
retrieval via :ref:`MPI_Abi_get_fortran_info`.  Note that, consistent
with the above, a stored ``mpi_<type><n>_supported=true`` echoed back by
:ref:`MPI_Abi_get_fortran_info` reflects what the application asserted
about its Fortran compiler, not necessarily that ``MPI_<TYPE><N>`` is
available in this build.

The composite Fortran datatypes (``MPI_2INTEGER``, ``MPI_2REAL``,
``MPI_2DOUBLE_PRECISION``, ``MPI_2COMPLEX``, ``MPI_2DOUBLE_COMPLEX``)
are not described by these keys and remain unavailable in a
``--disable-mpi-fortran`` build.

NOTES
-----

The application retains ownership of ``info`` and is responsible for freeing it via :ref:`MPI_Info_free`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Abi_get_fortran_info`
