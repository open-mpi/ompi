.. _mpi_error_string:


MPI_Error_string
================

.. include_body

:ref:`MPI_Error_string` |mdash| Returns a string for a given error code.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Error_string(int errorcode, char *string, int *resultlen)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ERROR_STRING(ERRORCODE, STRING, RESULTLEN, IERROR)
   	INTEGER		ERRORCODE, RESULTLEN, IERROR
   	CHARACTER*(*)	STRING


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Error_string(errorcode, string, resultlen, ierror)
   	INTEGER, INTENT(IN) :: errorcode
   	CHARACTER(LEN=MPI_MAX_ERROR_STRING), INTENT(OUT) :: string
   	INTEGER, INTENT(OUT) :: resultlen
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``errorcode``: Error code returned by an MPI routine or an MPI error class.

OUTPUT PARAMETERS
-----------------
* ``string``: Text that corresponds to the errorcode.
* ``resultlen``: Length of string.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns the error string associated with an error code or class. The
argument string must represent storage that is at least
MPI_MAX_ERROR_STRING characters long.

The number of characters actually written is returned in the output
argument, resultlen.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Error_class`
