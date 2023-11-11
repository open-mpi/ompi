.. _mpi_testany:


MPI_Testany
===========

.. include_body

:ref:`MPI_Testany` - Tests for completion of any one previously initiated
communication in a list.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Testany(int count, MPI_Request array_of_requests[],
   	int *index, int *flag, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TESTANY(COUNT, ARRAY_OF_REQUESTS, INDEX, FLAG, STATUS, IERROR)
   	LOGICAL	FLAG
   	INTEGER	COUNT, ARRAY_OF_REQUESTS(*), INDEX
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Testany(count, array_of_requests, index, flag, status, ierror)
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   	INTEGER, INTENT(OUT) :: index
   	LOGICAL, INTENT(OUT) :: flag
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: List length (integer).
* ``array_of_requests``: Array of requests (array of handles).

OUTPUT PARAMETERS
-----------------
* ``index``: Index of operation that completed, or ``MPI_UNDEFINED`` if none completed (integer).
* ``flag``: True if one of the operations is complete (logical).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Testany` tests for completion of either one or none of the operations
associated with active handles. In the former case, it returns *flag* =
true, returns in *index* the index of this request in the array, and
returns in *status* the status of that operation; if the request was
allocated by a nonblocking communication call then the request is
deallocated and the handle is set to ``MPI_REQUEST_NULL``. (The array is
indexed from 0 in C, and from 1 in Fortran.) In the latter case (no
operation completed), it returns *flag* = false, returns a value of
``MPI_UNDEFINED`` in *index*, and *status* is undefined.

The array may contain null or inactive handles. If the array contains no
active handles then the call returns immediately with *flag* = true,
*index* = ``MPI_UNDEFINED``, and an empty *status*.

If the array of requests contains active handles then the execution of
``MPI_Testany(count, array_of_requests, index, status)`` has the same effect
as the execution of ``MPI_Test(&array_of_requests[i], flag, status)``,
for *i*\ =0,1,...,count-1, in some arbitrary order, until one
call returns ``flag = true``, or all fail. In the former case, *index* is
set to the last value of *i*, and in the latter case, it is set to
``MPI_UNDEFINED``. :ref:`MPI_Testany` with an array containing one active entry is
equivalent to :ref:`MPI_Test`.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUS_IGNORE`` as a
special value for the *status* argument.


ERRORS
------

.. include:: ./ERRORS.rst

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on requests passed to
:ref:`MPI_TESTANY` do not set the ``status.MPI_ERROR`` field in the
returned *status*.  The error code is always passed to the back-end
error handler and may be passed back to the caller through the return
value of :ref:`MPI_TESTANY` if the back-end error handler returns it.
The pre-defined MPI error handler ``MPI_ERRORS_RETURN`` exhibits this
behavior, for example.

.. seealso::
   * :ref:`MPI_Comm_set_errhandler`
   * :ref:`MPI_File_set_errhandler`
   * :ref:`MPI_Test`
   * :ref:`MPI_Testall`
   * :ref:`MPI_Testsome`
   * :ref:`MPI_Wait`
   * :ref:`MPI_Waitall`
   * :ref:`MPI_Waitany`
   * :ref:`MPI_Waitsome`
   * :ref:`MPI_Win_set_errhandler`
