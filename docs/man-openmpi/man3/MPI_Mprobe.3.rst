.. _mpi_mprobe:


MPI_Mprobe
==========

.. include_body

:ref:`MPI_Mprobe` |mdash| Blocking matched probe for a message.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Mprobe(int source, int tag, MPI_Comm comm,
   	MPI_Message *message, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_MPROBE(SOURCE, TAG, COMM, MESSAGE, STATUS, IERROR)
   	INTEGER	SOURCE, TAG, COMM, MESSAGE
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Mprobe(source, tag, comm, message, status, ierror)
   	INTEGER, INTENT(IN) :: source, tag
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Message), INTENT(OUT) :: message
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``source``: Source rank or ``MPI_ANY_SOURCE`` (integer).
* ``tag``: Tag value or ``MPI_ANY_TAG`` (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``message``: Message (handle).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Like :ref:`MPI_Probe` and :ref:`MPI_Iprobe`, the :ref:`MPI_Mprobe` and :ref:`MPI_Improbe` operations
allow incoming messages to be queried without actually receiving them,
except that :ref:`MPI_Mprobe` and :ref:`MPI_Improbe` provide a mechanism to receive
the specific message that was matched regardless of other intervening
probe or receive operations. This gives the application an opportunity
to decide how to receive the message, based on the information returned
by the probe. In particular, the application may allocate memory for the
receive buffer according to the length of the probed message.

A matching probe with ``MPI_PROC_NULL`` as *source* returns *message* =
``MPI_MESSAGE_NO_PROC``, and the *status* object returns source =
``MPI_PROC_NULL``, tag = ``MPI_ANY_TAG``, and count = 0.

When :ref:`MPI_Mprobe` returns (from a non-``MPI_PROC_NULL`` *source*), the matched
message can then be received by passing the *message* handle to the
:ref:`MPI_Mrecv` or :ref:`MPI_Imrecv` functions.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Improbe`
   * :ref:`MPI_Probe`
   * :ref:`MPI_Iprobe`
   * :ref:`MPI_Mrecv`
   * :ref:`MPI_Imrecv`
   * :ref:`MPI_Cancel`
