.. _mpi_imrecv:


MPI_Imrecv
==========

.. include_body

:ref:`MPI_Imrecv` - Non-blocking receive for a matched message


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Imrecv(void *buf, int count, MPI_Datatype type,
   	MPI_Message *message, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_IMRECV(BUF, COUNT, DATATYPE, MESSAGE, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	COUNT, DATATYPE, MESSAGE, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Imrecv(buf, count, datatype, message, request, ierror)
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Message), INTENT(INOUT) :: message
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: Number of elements to receive (nonnegative integer).
* ``datatype``: Datatype of each send buffer element (handle).
* ``message``: Message (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of receive buffer (choice).
* ``request``: Request (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The functions :ref:`MPI_Mrecv` and :ref:`MPI_Imrecv` receive messages that have been
previously matched by a matching probe.

The *request* returned from :ref:`MPI_Imrecv` can be used with any of the
:ref:`MPI_Test` and :ref:`MPI_Wait` variants, like any non-blocking receive request.

If :ref:`MPI_Imrecv` is called with ``MPI_MESSAGE_NULL`` as the message argument, a
call to one of the :ref:`MPI_Test` or :ref:`MPI_Wait` variants will return immediately
with the *status* object set to *source* = ``MPI_PROC_NULL``, *tag* =
``MPI_ANY_TAG``, and *count* = 0, as if a receive from ``MPI_PROC_NULL`` was
issued.

If reception of a matched message is started with :ref:`MPI_Imrecv`, then it is
possible to cancel the returned request with :ref:`MPI_Cancel`. If :ref:`MPI_Cancel`
succeeds, the matched message must be found by a subsequent message
probe (:ref:`MPI_Probe`, :ref:`MPI_Iprobe`, :ref:`MPI_Mprobe`, or :ref:`MPI_Improbe`), received by a
subsequent receive operation or canceled by the sender.

Note, however, that is it possible for the cancellation of operations
initiated with :ref:`MPI_Imrecv` to fail. An example of a failing case is when
canceling the matched message receive would violate MPI message ordering
rules (e.g., if another message matching the same message signature has
matched |mdash| and possibly received |mdash| before this :ref:`MPI_Imrecv` is canceled).

If your application does not need to examine the *status* field, you
can save resources by using the predefined constant
``MPI_STATUS_IGNORE`` as a special value for the *status* argument.


ERRORS
------

.. include:: ./ERRORS.rst

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on messages received
by :ref:`MPI_Imrecv` do not set the ``status.MPI_ERROR`` field in the
returned *status*.  The error code is always passed to the back-end
error handler and may be passed back to the caller through the return
value of :ref:`MPI_Imrecv` if the back-end error handler returns it.
The pre-defined MPI error handler ``MPI_ERRORS_RETURN`` exhibits this
behavior, for example.

.. seealso::
   * :ref:`MPI_Mprobe`
   * :ref:`MPI_Improbe`
   * :ref:`MPI_Probe`
   * :ref:`MPI_Iprobe`
   * :ref:`MPI_Imrecv`
   * :ref:`MPI_Cancel`
