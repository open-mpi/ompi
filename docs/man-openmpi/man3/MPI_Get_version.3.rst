.. _mpi_get_version:

MPI_Get_version
===============

.. include_body

:ref:`MPI_Get_version` - Returns the version of the standard corresponding
to the current implementation.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Get_version(int *version, int *subversion)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_VERSION(VERSION, SUBVERSION, IERROR)
       INTEGER VERSION, SUBVERSION, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Get_version(version, subversion, ierror)
       INTEGER, INTENT(OUT) :: version, subversion
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Output Parameters
-----------------

-  ``version`` : The major version number of the corresponding standard
   (integer).
-  ``subversion`` : The minor version number of the corresponding
   standard (integer).
-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

Since Open MPI is MPI 3.1 compliant, this function will return a
``version`` value of 3 and a subversion value of 1 for this release.

Note
----

:ref:`MPI_Get_version` is one of the few functions that can be called
before :ref:`MPI_Init` and after :ref:`MPI_Finalize`.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
