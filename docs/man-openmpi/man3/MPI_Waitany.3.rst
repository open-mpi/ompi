.. _mpi_waitany:


MPI_Waitany
===========

.. include_body

:ref:`MPI_Waitany` - Waits for any specified send or receive to complete.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Waitany(int count, MPI_Request array_of_requests[],
   	int *index, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WAITANY(COUNT, ARRAY_OF_REQUESTS, INDEX, STATUS, IERROR)
   	INTEGER	COUNT, ARRAY_OF_REQUESTS(*), INDEX
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Waitany(count, array_of_requests, index, status, ierror)
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   	INTEGER, INTENT(OUT) :: index
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: List length (integer).
* ``array_of_requests``: Array of requests (array of handles).

OUTPUT PARAMETERS
-----------------
* ``index``: Index of handle for operation that completed (integer). In the range 0 to count-1. In Fortran, the range is 1 to count.
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

A call to :ref:`MPI_Waitany` can be used to wait for the completion of one out
of several requests.

The array_of_requests list may contain null or inactive handles. If the
list contains no active handles (list has length zero or all entries are
null or inactive), then the call returns immediately with index =
``MPI_UNDEFINED``, and an empty status.

The execution of MPI_Waitany(count, array_of_requests, index, status)
has the same effect as the execution of ``MPI_Wait(&array_of_requests[i], status)``,
where i is the value returned by index (unless the value of
index is ``MPI_UNDEFINED``). :ref:`MPI_Waitany` with an array containing one active
entry is equivalent to :ref:`MPI_Wait`.

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUS_IGNORE`` as a
special value for the *status* argument.

**Example:** Client-server code (starvation can occur).

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
                 CALL MPI_IRECV(a(1,i), n, MPI_REAL, i tag, &
                          comm, request_list(i), ierr)
              END DO
              DO
                 CALL MPI_WAITANY(size-1, request_list, index, status, ierr)
                 CALL DO_SERVICE(a(1,index))  ! handle one message
                 CALL MPI_IRECV(a(1, index), n, MPI_REAL, index, tag, &
                           comm, request_list(index), ierr)
              END DO
       END IF


ERRORS
------

.. include:: ./ERRORS.rst

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on requests passed to
:ref:`MPI_WAITANY` do not set the ``status.MPI_ERROR`` field in the
returned *status*.  The error code is always passed to the back-end
error handler and may be passed back to the caller through the return
value of :ref:`MPI_WAITANY` if the back-end error handler returns it.
The pre-defined MPI error handler ``MPI_ERRORS_RETURN`` exhibits this
behavior, for example.


.. seealso::
   * :ref:`MPI_Comm_set_errhandler`
   * :ref:`MPI_File_set_errhandler`
   * :ref:`MPI_Test`
   * :ref:`MPI_Testall`
   * :ref:`MPI_Testany`
   * :ref:`MPI_Testsome`
   * :ref:`MPI_Wait`
   * :ref:`MPI_Waitall`
   * :ref:`MPI_Waitsome`
   * :ref:`MPI_Win_set_errhandler`
