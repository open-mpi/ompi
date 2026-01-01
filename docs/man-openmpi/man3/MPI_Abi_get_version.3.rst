.. _mpi_abi_get_version:

MPI_ABI_get_version
===================

.. include_body

:ref:`MPI_Abi_get_version` |mdash| Returns the standard ABI version, if supported.

.. The following file was automatically generated
.. include:: ./bindings/mpi_abi_get_version.rst

OUTPUT PARAMETERS
-----------------

* ``abi_major`` : The ABI major version number (integer).
* ``abi_minor`` : The ABI minor version number (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Abi_get_version` returns the standard ABI version, if supported. Otherwise,
the values of the major and minor version are set to −1. The ABI version is independent
of the MPI specification version. The major and minor version of the ABI associated
with MPI-5.0 are 1 and 0.

The ABI version macros MPI_ABI_VERSION and MPI_ABI_SUBVERSION are present in the MPI header and modules
so that applications can check for consistency between the compilation
environment and the properties of the implementation at runtime.

NOTE
----

:ref:`MPI_Abi_get_version` is one of the few functions that can be called
before :ref:`MPI_Init` and after :ref:`MPI_Finalize`.

ERRORS
------

.. include:: ./ERRORS.rst
