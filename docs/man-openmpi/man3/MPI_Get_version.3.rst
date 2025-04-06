.. _mpi_get_version:

MPI_Get_version
===============

.. include_body

:ref:`MPI_Get_version` |mdash| Returns the version of the standard corresponding
to the current implementation.

.. The following file was automatically generated
.. include:: ./bindings/mpi_get_version.rst

OUTPUT PARAMETERS
-----------------

* ``version`` : The major version number of the corresponding standard
  (integer).
* ``subversion`` : The minor version number of the corresponding
  standard (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Since Open MPI |ompi_ver| is MPI-|mpi_standard_version| compliant (see
:ref:`this page for more details about Open MPI standards conformance
<release-notes-mpi-standard-conformance-label>`) this function will return a
``version`` value of |mpi_standard_major_version| and a ``subversion``
value of |mpi_standard_minor_version| for this release.

NOTE
----

:ref:`MPI_Get_version` is one of the few functions that can be called
before :ref:`MPI_Init` and after :ref:`MPI_Finalize`.

ERRORS
------

.. include:: ./ERRORS.rst
