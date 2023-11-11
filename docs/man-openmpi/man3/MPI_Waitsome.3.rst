.. _mpi_waitsome:


MPI_Waitsome
============

.. include_body

:ref:`MPI_Waitsome` |mdash| Waits for some given communications to complete.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Waitsome(int incount, MPI_Request array_of_requests[],
   	int *outcount, int array_of_indices[],
   	MPI_Status array_of_statuses[])


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WAITSOME(INCOUNT, ARRAY_OF_REQUESTS, OUTCOUNT,
   		ARRAY_OF_INDICES, ARRAY_OF_STATUSES, IERROR)
   	INTEGER	INCOUNT, ARRAY_OF_REQUESTS(*), OUTCOUNT
   	INTEGER	ARRAY_OF_INDICES(*)
   	INTEGER	ARRAY_OF_STATUSES(MPI_STATUS_SIZE*)
   	INTEGER	IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Waitsome(incount, array_of_requests, outcount, array_of_indices,
   		array_of_statuses, ierror)
   	INTEGER, INTENT(IN) :: incount
   	TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
   	INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
   	TYPE(MPI_Status) :: array_of_statuses(*)
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``incount``: Length of array_of_requests (integer).
* ``array_of_requests``: Array of requests (array of handles).

OUTPUT PARAMETERS
-----------------
* ``outcount``: Number of completed requests (integer).
* ``array_of_indices``: Array of indices of operations that completed (array of integers).
* ``array_of_statuses``: Array of status objects for operations that completed (array of status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Waits until at least one of the operations associated with active
handles in the list have completed. Returns in outcount the number of
requests from the list array_of_requests that have completed. Returns in
the first outcount locations of the array array_of_indices the indices
of these operations (index within the array array_of_requests; the array
is indexed from 0 in C and from 1 in Fortran). Returns in the first
outcount locations of the array array_of_status the status for these
completed operations. If a request that completed was allocated by a
nonblocking communication call, then it is deallocated, and the
associated handle is set to MPI_REQUEST_NULL.

If the list contains no active handles, then the call returns
immediately with outcount = MPI_UNDEFINED.

When one or more of the communications completed by :ref:`MPI_Waitsome` fails,
then it is desirable to return specific information on each
communication. The arguments outcount, array_of_indices, and
array_of_statuses will be adjusted to indicate completion of all
communications that have succeeded or failed. The call will return the
error code MPI_ERR_IN_STATUS and the error field of each status returned
will be set to indicate success or to indicate the specific error that
occurred. The call will return MPI_SUCCESS if no request resulted in an
error, and will return another error code if it failed for other reasons
(such as invalid arguments). In such cases, it will not update the error
fields of the statuses.

If your application does not need to examine the *array_of_statuses*
field, you can save resources by using the predefined constant
MPI_STATUSES_IGNORE can be used as a special value for the
*array_of_statuses* argument.

**Example:** Same code as the example in the :ref:`MPI_Waitany` man page, but
using :ref:`MPI_Waitsome`.

.. code-block:: fortran

       CALL MPI_COMM_SIZE(comm, size, ierr)
       CALL MPI_COMM_RANK(comm, rank, ierr)
       IF(rank > 0) THEN         ! client code
           DO
              CALL MPI_ISEND(a, n, MPI_REAL, 0, tag, comm, request, ierr)
              CALL MPI_WAIT(request, status, ierr)
           END DO
       ELSE         ! rank=0 -- server code
           DO i=1, size-1
              CALL MPI_IRECV(a(1,i), n, MPI_REAL, i, tag, &
                             comm, requests(i), ierr)
           END DO
           DO
              CALL MPI_WAITSOME(size, request_list, numdone, &
                               indices, statuses, ierr)
              DO i=1, numdone
                 CALL DO_SERVICE(a(1, indices(i)))
                 CALL MPI_IRECV(a(1, indices(i)), n, MPI_REAL, i, tag, &
                              comm, requests(indices(i)), ierr)
              END DO
           END DO
       END IF


NOTES
-----

The array of indices are in the range 0 to incount-1 for C and in the
range 1 to incount for Fortran.


ERRORS
------

.. include:: ./ERRORS.rst

For each invocation of :ref:`MPI_Waitsome`, if one or more requests
generate an MPI error, only the *first* MPI request that caused an
error will be passed to its corresponding error handler. No other
error handlers will be invoked (even if multiple requests generated
errors). However, *all* requests that generate an error will have a
relevant error code set in the corresponding ``status.MPI_ERROR``
field (unless ``MPI_STATUSES_IGNORE`` was used).

If the invoked error handler allows :ref:`MPI_Waitsome` to return to
the caller, the value ``MPI_ERR_IN_STATUS`` will be returned in the C
and Fortran bindings.


.. seealso::
   * :ref:`MPI_Comm_set_errhandler`
   * :ref:`MPI_File_set_errhandler`
   * :ref:`MPI_Test`
   * :ref:`MPI_Testall`
   * :ref:`MPI_Testany`
   * :ref:`MPI_Testsome`
   * :ref:`MPI_Wait`
   * :ref:`MPI_Waitall`
   * :ref:`MPI_Waitany`
   * :ref:`MPI_Win_set_errhandler`
