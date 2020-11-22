.. _mpi_add_error_string:


MPI_Add_error_string
====================

.. include_body

::

   MPI_Add_error_string - Associates a string with an error code or class


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Add_error_string(int errorcode, const char *string)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ADD_ERROR_STRING(ERRORCODE, STRING, IERROR)
   	INTEGER		ERRORCODE, IERROR
   	CHARACTER*(*)	STRING


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Add_error_string(errorcode, string, ierror)
   	INTEGER, INTENT(IN) :: errorcode
   	CHARACTER(LEN=*), INTENT(IN) :: string
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``errorcode``: MPI error class, or an error code returned by an MPI routine (integer).
* ``string``: Text that corresponds to the error code or class (string).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine associates an error string with an error code or class.
Calling :ref:`MPI_Add_error_string` for an error code or class that already has
an associated error string will replace the old string with the new one.
It is erroneous to call :ref:`MPI_Add_error_string` for an error value not
generated via :ref:`MPI_Add_error_class` or :ref:`MPI_Add_error_code` (e.g., an error
code or class with a value not greater than MPI_LAST_ERRCODE).


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Add_error_class` :ref:`MPI_Add_error_code` :ref:`MPI_Error_class` :ref:`MPI_Error_string`
