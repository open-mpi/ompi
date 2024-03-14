.. _mpi_add_error_code:


MPI_Add_error_code
==================

.. include_body

:ref:`MPI_Add_error_code` |mdash| Creates a new error code associated with
*errorclass*


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Add_error_code(int errorclass, int *errorcode)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ADD_ERROR_CODE(ERRORCLASS, ERRORCODE, IERROR)
   	INTEGER  ERRORCLASS, ERRORCODE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Add_error_code(errorclass, errorcode, ierror)
   	INTEGER, INTENT(IN) :: errorclass
   	INTEGER, INTENT(OUT) :: errorcode
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``errorclass``: MPI error class (integer).

OUTPUT PARAMETERS
-----------------
* ``errorcode``: Error code returned by an MPI routine or an MPI error class (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Creates a new error code associated with *errorclass* and returns its
value in *errorcode*.


NOTES
-----

No function is provided to free error codes, as it is not expected that
an application will create them in significant numbers.

The value returned is always greater than or equal to MPI_ERR_LASTCODE.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Add_error_class`
   * :ref:`MPI_Error_class`
