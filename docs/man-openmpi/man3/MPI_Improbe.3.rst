.. _mpi_improbe:


MPI_Improbe
===========

.. include_body

:ref:`MPI_Improbe` |mdash| Non-blocking matched probe for a message.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Improbe(int source, int tag, MPI_Comm comm,
   	int *flag, MPI_Message *message, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_IMPROBE(SOURCE, TAG, COMM, FLAG, MESSAGE, STATUS, IERROR)
   	LOGICAL	FLAG
   	INTEGER	SOURCE, TAG, COMM, MESSAGE
   	INTEGER	STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Improbe(source, tag, comm, flag, message, status, ierror)
   	INTEGER, INTENT(IN) :: source, tag
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(OUT) :: flag
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
* ``flag``: Flag (logical).
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

A matching probe with ``MPI_PROC_NULL`` as *source* returns *flag* = true,

*message* = ``MPI_MESSAGE_NO_PROC``, and the *status* object returns source
^ ``MPI_PROC_NULL``, tag ^ ``MPI_ANY_TAG``, and count ^ 0.

:ref:`MPI_Iprobe` returns a true value in *flag* if a message has been matched
and can be received by passing the *message* handle to the :ref:`MPI_Mrecv` or
:ref:`MPI_Imrecv` functions, provided the *source* was not ``MPI_PROC_NULL``.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Mprobe`
   * :ref:`MPI_Probe`
   * :ref:`MPI_Iprobe`
   * :ref:`MPI_Mrecv`
   * :ref:`MPI_Imrecv`
   * :ref:`MPI_Cancel`
