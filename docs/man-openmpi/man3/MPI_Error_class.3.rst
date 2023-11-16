.. _mpi_error_class:


MPI_Error_class
===============

.. include_body

:ref:`MPI_Error_class` |mdash| Converts an error code into an error class.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Error_class(int errorcode, int *errorclass)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ERROR_CLASS(ERRORCODE, ERRORCLASS, IERROR)
   	INTEGER	ERRORCODE, ERRORCLASS, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Error_class(errorcode, errorclass, ierror)
   	INTEGER, INTENT(IN) :: errorcode
   	INTEGER, INTENT(OUT) :: errorclass
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``errorcode``: Error code returned by an MPI routine.

OUTPUT PARAMETERS
-----------------
* ``errorclass``: Error class associated with errorcode.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Error_class` maps each standard error code (error class)
onto itself.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Error_string`
