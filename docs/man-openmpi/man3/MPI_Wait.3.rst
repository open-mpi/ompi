.. _mpi_wait:


MPI_Wait
========

.. include_body

:ref:`MPI_Wait` - Waits for an MPI send or receive to complete.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Wait(MPI_Request *request, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WAIT(REQUEST, STATUS, IERROR)
   	INTEGER	REQUEST, STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Wait(request, status, ierror)
   	TYPE(MPI_Request), INTENT(INOUT) :: request
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``request``: Request (handle).

OUTPUT PARAMETERS
-----------------
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

A call to :ref:`MPI_Wait` returns when the operation identified by request is
complete. If the communication object associated with this request was
created by a nonblocking send or receive call, then the object is
deallocated by the call to :ref:`MPI_Wait` and the request handle is set to
``MPI_REQUEST_NULL``.

The call returns, in status, information on the completed operation. The
content of the status object for a receive operation can be accessed as
described in Section 3.2.5 of the MPI-1 Standard, "Return Status." The
status object for a send operation may be queried by a call to
:ref:`MPI_Test_cancelled` (see Section 3.8 of the MPI-1 Standard, "Probe and
Cancel").

If your application does not need to examine the *status* field, you can
save resources by using the predefined constant ``MPI_STATUS_IGNORE`` as a
special value for the *status* argument.

One is allowed to call :ref:`MPI_Wait` with a null or inactive request
argument. In this case the operation returns immediately with empty
status.


NOTES
-----

Successful return of :ref:`MPI_Wait` after an :ref:`MPI_Ibsend` implies that the user
send buffer can be reused i.e., data has been sent out or copied into a
buffer attached with :ref:`MPI_Buffer_attach`. Note that, at this point, we can
no longer cancel the send (for more information, see Section 3.8 of the
MPI-1 Standard, "Probe and Cancel"). If a matching receive is never
posted, then the buffer cannot be freed. This runs somewhat counter to
the stated goal of :ref:`MPI_Cancel` (always being able to free program space
that was committed to the communication subsystem).

Example: Simple usage of nonblocking operations and :ref:`MPI_Wait`.

.. code-block:: fortran

       CALL MPI_COMM_RANK(comm, rank, ierr)
       IF(rank == 0) THEN
           CALL MPI_ISEND(a(1), 10, MPI_REAL, 1, tag, comm, request, ierr)
           **** do some computation ****
           CALL MPI_WAIT(request, status, ierr)
       ELSE IF (rank == 1) THEN
           CALL MPI_IRECV(a(1), 15, MPI_REAL, 0, tag, comm, request, ierr)
           **** do some computation ****
           CALL MPI_WAIT(request, status, ierr)
       END IF


ERRORS
------

.. include:: ./ERRORS.rst 

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on requests passed to
:ref:`MPI_WAIT` do not set the ``status.MPI_ERROR`` field in the
returned *status*.  The error code is always passed to the back-end
error handler and may be passed back to the caller through the return
value of :ref:`MPI_WAIT` if the back-end error handler returns it.  The
pre-defined MPI error handler ``MPI_ERRORS_RETURN`` exhibits this
behavior, for example.


.. seealso::
   * :ref:`MPI_Comm_set_errhandler`
   * :ref:`MPI_File_set_errhandler`
   * :ref:`MPI_Test`
   * :ref:`MPI_Testall`
   * :ref:`MPI_Testany`
   * :ref:`MPI_Testsome`
   * :ref:`MPI_Waitall`
   * :ref:`MPI_Waitany`
   * :ref:`MPI_Waitsome`
   * :ref:`MPI_Win_set_errhandler`
