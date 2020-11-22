.. _mpi_test_cancelled:


MPI_Test_cancelled
==================

.. include_body

:ref:`MPI_Test_cancelled` - Tests whether a request was canceled.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Test_cancelled(const MPI_Status *status, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TEST_CANCELLED(STATUS, FLAG, IERROR)
   	LOGICAL	FLAG
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Test_cancelled(status, flag, ierror)
   	TYPE(MPI_Status), INTENT(IN) :: status
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``status``: Status object (status).

OUTPUT PARAMETERS
-----------------
* ``flag``: True if operation was cancelled (logical).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns *flag* = true if the communication associated with the status
object was canceled successfully. In such a case, all other fields of
status (such as *count* or *tag*) are undefined. Otherwise, returns
*flag* = false. If a receive operation might be canceled, one should
call :ref:`MPI_Test_cancelled` first, to check whether the operation was
canceled, before checking on the other fields of the return status.


NOTES
-----

Cancel can be an expensive operation that should be used only
exceptionally.


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
