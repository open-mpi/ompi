.. _mpi_send:


MPI_Send
========

.. include_body

:ref:`MPI_Send` |mdash| Performs a standard-mode blocking send.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Send(const void *buf, int count, MPI_Datatype datatype, int dest,
   	int tag, MPI_Comm comm)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)
   	<type>	BUF(*)
   	INTEGER	COUNT, DATATYPE, DEST, TAG, COMM, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Send(buf, count, datatype, dest, tag, comm, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: buf
   	INTEGER, INTENT(IN) :: count, dest, tag
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``buf``: Initial address of send buffer (choice).
* ``count``: Number of elements send (nonnegative integer).
* ``datatype``: Datatype of each send buffer element (handle).
* ``dest``: Rank of destination (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Send` performs a standard-mode, blocking send.


NOTE
----

This routine will block until the message is sent to the destination.
For an in-depth explanation of the semantics of the standard-mode send,
refer to the `MPI Standard <https://www.mpi-forum.org/docs/>`_.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Isend`
   * :ref:`MPI_Bsend`
   * :ref:`MPI_Recv`
