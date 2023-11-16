.. _mpi_get_library_version:

MPI_Get_library_version
=======================

.. include_body

:ref:`MPI_Get_library_version` |mdash| Returns a string of the current Open MPI
version

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Get_library_version(char *version, int *resultlen)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_LIBRARY_VERSION(VERSION, RESULTLEN, IERROR)
       CHARACTER*(*)   NAME
       INTEGER RESULTLEN, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Get_library_version(version, resulten, ierror)
       CHARACTER(LEN=MPI_MAX_LIBRARY_VERSION_STRING), INTENT(OUT) :: version
       INTEGER, INTENT(OUT) :: resultlen
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

OUTPUT PARAMETERS
-----------------

* ``version`` : A string containing the Open MPI version (string).
* ``resultlen`` : Length (in characters) of result returned in version
   (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine returns a string representing the version of the MPI
library. The version argument is a character string for maximum
flexibility.

The number of characters actually written is returned in the output
argument, resultlen. In C, a '0' character is additionally stored at
version[resultlen]. The resultlen cannot be larger than
(MPI_MAX_LIBRARY_VERSION_STRING - 1). In Fortran, version is padded on
the right with blank characters. The resultlen cannot be larger than
MPI_MAX_LIBRARY_VERSION_STRING.

NOTE
----

The version string that is passed must be at least
MPI_MAX_LIBRARY_VERSION_STRING characters long.

:ref:`MPI_Get_library_version` is one of the few functions that can be called
before :ref:`MPI_Init` and after :ref:`MPI_Finalize`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Get_version`
