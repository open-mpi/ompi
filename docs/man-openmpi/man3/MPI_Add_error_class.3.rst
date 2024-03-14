.. _mpi_add_error_class:


MPI_Add_error_class
===================

.. include_body

:ref:`MPI_Add_error_class` |mdash| Creates a new error class and returns its value


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Add_error_class(int *errorclass)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ADD_ERROR_CLASS(ERRORCLASS, IERROR)
   	INTEGER	ERRORCLASS, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Add_error_class(errorclass, ierror)
   	INTEGER, INTENT(OUT) :: errorclass
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


OUTPUT PARAMETERS
-----------------
* ``errorclass``: New error class (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Add_error_class` creates a new, local error class.


NOTES
-----

Because this function is local, the same value of *errorclass* may not
be returned on all processes that make this call, even if they call the
function concurrently. Thus, same error on different processes may not
cause the same value of *errorclass* to be returned. To reduce the
potential for confusion, :ref:`MPI_Add_error_string` may be used on multiple
processes to associate the same error string with the newly created
*errorclass*. Even though *errorclass* may not be consistent across
processes, using :ref:`MPI_Add_error_string` will ensure the error string
associated with it will be the same everywhere.

No function is provided to free error classes, as it is not expected
that an application will create them in significant numbers.

The value returned is always greater than or equal to MPI_ERR_LASTCODE.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Add_error_code`
   * :ref:`MPI_Add_error_string`
   * :ref:`MPI_Error_class`
   * :ref:`MPI_Error_string`
