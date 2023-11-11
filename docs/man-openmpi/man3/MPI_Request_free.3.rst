.. _mpi_request_free:


MPI_Request_free
================

.. include_body

:ref:`MPI_Request_free` |mdash| Frees a communication request object.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Request_free(MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_REQUEST_FREE(REQUEST, IERROR)
   	INTEGER	REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Request_free(request, ierror)
   	TYPE(MPI_Request), INTENT(INOUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``request``: Communication request (handle).

DESCRIPTION
-----------

This operation allows a request object to be deallocated without waiting
for the associated communication to complete.

:ref:`MPI_Request_free` marks the request object for deallocation and sets
request to MPI_REQUEST_NULL. Any ongoing communication that is
associated with the request will be allowed to complete. The request
will be deallocated only after its completion.


NOTES
-----

Once a request is freed by a call to :ref:`MPI_Request_free`, it is not
possible to check for the successful completion of the associated
communication with calls to :ref:`MPI_Wait` or :ref:`MPI_Test`. Also, if an error
occurs subsequently during the communication, an error code cannot be
returned to the user -- such an error must be treated as fatal.
Questions arise as to how one knows when the operations have completed
when using :ref:`MPI_Request_free`. Depending on the program logic, there may
be other ways in which the program knows that certain operations have
completed and this makes usage of :ref:`MPI_Request_free` practical. For
example, an active send request could be freed when the logic of the
program is such that the receiver sends a reply to the message sent --
the arrival of the reply informs the sender that the send has completed
and the send buffer can be reused. An active receive request should
never be freed, as the receiver will have no way to verify that the
receive has completed and the receive buffer can be reused.

**Example:**

.. code-block:: fortran

       CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank)
       IF(rank == 0) THEN
           DO i=1, n
             CALL MPI_ISEND(outval, 1, MPI_REAL, 1, 0, req, ierr)
             CALL MPI_REQUEST_FREE(req, ierr)
             CALL MPI_IRECV(inval, 1, MPI_REAL, 1, 0, req, ierr)
             CALL MPI_WAIT(req, status, ierr)
           END DO
       ELSE IF (rank == 1) THEN
           CALL MPI_IRECV(inval, 1, MPI_REAL, 0, 0, req, ierr)
           CALL MPI_WAIT(req, status)
           DO I=1, n-1
              CALL MPI_ISEND(outval, 1, MPI_REAL, 0, 0, req, ierr)
              CALL MPI_REQUEST_FREE(req, ierr)
              CALL MPI_IRECV(inval, 1, MPI_REAL, 0, 0, req, ierr)
              CALL MPI_WAIT(req, status, ierr)
           END DO
           CALL MPI_ISEND(outval, 1, MPI_REAL, 0, 0, req, ierr)
           CALL MPI_WAIT(req, status)
       END IF

This routine is normally used to free persistent requests created with
either :ref:`MPI_Recv_init` or :ref:`MPI_Send_init` and friends. However, it can
be used to free a request created with :ref:`MPI_Irecv` or :ref:`MPI_Isend` and
friends; in that case the use can not use the test/wait routines on the
request.

It **is** permitted to free an active request. However, once freed, you
can not use the request in a wait or test routine (e.g., :ref:`MPI_Wait` ).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Isend`
   * :ref:`MPI_Irecv`
   * :ref:`MPI_Issend`
   * :ref:`MPI_Ibsend`
   * :ref:`MPI_Irsend`
   * :ref:`MPI_Recv_init`
   * :ref:`MPI_Send_init`
   * :ref:`MPI_Ssend_init`
   * :ref:`MPI_Rsend_init`
   * :ref:`MPI_Test`
   * :ref:`MPI_Wait`
   * :ref:`MPI_Waitall`
   * :ref:`MPI_Waitany`
   * :ref:`MPI_Waitsome`
   * :ref:`MPI_Testall`
   * :ref:`MPI_Testany`
   * :ref:`MPI_Testsome`
