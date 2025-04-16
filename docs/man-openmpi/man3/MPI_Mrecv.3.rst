.. _mpi_mrecv:


MPI_Mrecv
=========

.. include_body

:ref:`MPI_Mrecv` |mdash| Blocking receive for a matched message

.. The following file was automatically generated
.. include:: ./bindings/mpi_mrecv.rst

INPUT PARAMETERS
----------------
* ``count``: Number of elements to receive (nonnegative integer).
* ``datatype``: Datatype of each send buffer element (handle).
* ``message``: Message (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of receive buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The functions :ref:`MPI_Mrecv` and :ref:`MPI_Imrecv` receive messages that have been
previously matched by a matching probe.

If :ref:`MPI_Mrecv` is called with ``MPI_MESSAGE_NULL`` as the message argument,
the call returns immediately with the *status* object set to *source* =
``MPI_PROC_NULL``, *tag* = ``MPI_ANY_TAG``, and *count* = 0, as if a receive
from ``MPI_PROC_NULL`` was issued.

If your application does not need to examine the *status* field, you
can save resources by using the predefined constant
``MPI_STATUS_IGNORE`` as a special value for the *status* argument.


ERRORS
------

.. include:: ./ERRORS.rst

Note that per the "Return Status" section in the "Point-to-Point
Communication" chapter in the `MPI Standard
<https://www.mpi-forum.org/docs/>`_, MPI errors on messages received
by :ref:`MPI_Mrecv` do not set the ``status.MPI_ERROR`` field in the
returned *status*.  The error code is always passed to the back-end
error handler and may be passed back to the caller through the return
value of :ref:`MPI_Mrecv` if the back-end error handler returns it.
The pre-defined MPI error handler ``MPI_ERRORS_RETURN`` exhibits this
behavior, for example.

.. seealso::
   * :ref:`MPI_Mprobe`
   * :ref:`MPI_Improbe`
   * :ref:`MPI_Probe`
   * :ref:`MPI_Iprobe`
   * :ref:`MPI_Imrecv`
   * :ref:`MPI_Cancel`
