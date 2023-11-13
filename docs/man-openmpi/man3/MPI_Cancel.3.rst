.. _mpi_cancel:

MPI_Cancel
==========

.. include_body

:ref:`MPI_Cancel` |mdash| Cancels a communication request.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Cancel(MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CANCEL(REQUEST, IERROR)
       INTEGER REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Cancel(request, ierror)
       TYPE(MPI_Request), INTENT(IN) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
---------------

* ``request`` : Communication request (handle).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

The :ref:`MPI_Cancel` operation allows pending communications to be canceled.
This is required for cleanup. Posting a send or a receive ties up user
resources (send or receive buffers), and a cancel may be needed to free
these resources gracefully.

A call to :ref:`MPI_Cancel` marks for cancellation a pending, nonblocking
communication operation (send or receive). The cancel call is local. It
returns immediately, possibly before the communication is actually
canceled. It is still necessary to complete a communication that has
been marked for cancellation, using a call to :ref:`MPI_Request_free`,
:ref:`MPI_Wait`, or :ref:`MPI_Test` (or any of the derived operations).

If a communication is marked for cancellation, then an :ref:`MPI_Wait` call for
that communication is guaranteed to return, irrespective of the
activities of other processes (i.e., :ref:`MPI_Wait` behaves as a local
function); similarly if :ref:`MPI_Test` is repeatedly called in a busy wait
loop for a canceled communication, then :ref:`MPI_Test` will eventually be
successful.

:ref:`MPI_Cancel` can be used to cancel a communication that uses a persistent
request (see Section 3.9 in the `MPI Standard <https://www.mpi-forum.org/docs/>`_, "Persistent
Communication Requests") in the same way it is used for nonpersistent
requests. A successful cancellation cancels the active communication,
but not the request itself. After the call to :ref:`MPI_Cancel` and the
subsequent call to :ref:`MPI_Wait` or :ref:`MPI_Test`, the request becomes inactive
and can be activated for a new communication.

The successful cancellation of a buffered send frees the buffer space
occupied by the pending message.

Either the cancellation succeeds or the communication succeeds, but not
both. If a send is marked for cancellation, then it must be the case
that either the send completes normally, in which case the message sent
is received at the destination process, or that the send is successfully
canceled, in which case no part of the message is received at the
destination. Then, any matching receive has to be satisfied by another
send. If a receive is marked for cancellation, then it must be the case
that either the receive completes normally, or that the receive is
successfully canceled, in which case no part of the receive buffer is
altered. Then, any matching send has to be satisfied by another receive.

If the operation has been canceled, then information to that effect will
be returned in the status argument of the operation that completes the
communication.

NOTES
-----

The primary expected use of :ref:`MPI_Cancel` is in multi-buffering schemes,
where speculative MPI_Irecvs are made. When the computation completes,
some of these requests may remain; using :ref:`MPI_Cancel` allows the user to
cancel these unsatisfied requests.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Probe`
