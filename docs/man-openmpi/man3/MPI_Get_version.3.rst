.. _mpi_get_version:

MPI_Get_version
===============

.. include_body

:ref:`MPI_Get_version` |mdash| Returns the version of the standard corresponding
to the current implementation.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Get_version(int *version, int *subversion)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_VERSION(VERSION, SUBVERSION, IERROR)
       INTEGER VERSION, SUBVERSION, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Get_version(version, subversion, ierror)
       INTEGER, INTENT(OUT) :: version, subversion
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
