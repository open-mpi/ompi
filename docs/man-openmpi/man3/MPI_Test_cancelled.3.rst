.. _mpi_test_cancelled:


MPI_Test_cancelled
==================

.. include_body

:ref:`MPI_Test_cancelled` |mdash| Tests whether a request was canceled.


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
* ``ierror``: Fortran only: Error status (integer).

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

.. include:: ./ERRORS.rst
