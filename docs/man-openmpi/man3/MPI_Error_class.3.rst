.. _mpi_error_class:


MPI_Error_class
===============

.. include_body

:ref:`MPI_Error_class` - Converts an error code into an error class.


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
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Error_class` maps each standard error code (error class)
onto itself.


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
   :ref:`MPI_Error_string`
