.. _mpi_bsend_init:

MPI_Bsend_init
==============

.. include_body

:ref:`MPI_Bsend_init` |mdash| Builds a handle for a buffered send.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Bsend_init(const void *buf, int count, MPI_Datatype datatype,
       int dest, int tag, MPI_Comm comm, MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_BSEND_INIT(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
       <type>  BUF(*)
       INTEGER COUNT, DATATYPE, DEST, TAG,
       INTEGER COMM, REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Bsend_init(buf, count, datatype, dest, tag, comm, request, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
       INTEGER, INTENT(IN) :: count, dest, tag
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Request), INTENT(OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``buf`` : Initial address of send buffer (choice).
* ``count`` : Number of elements sent (integer).
* ``datatype`` : Type of each element (handle).
* ``dest`` : Rank of destination (integer).
* ``tag`` : Message tag (integer).
* ``comm`` : Communicator (handle).

OUTPUT PARAMETERS
-----------------

* ``request`` : Communication request (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Creates a persistent communication request for a buffered mode send, and
binds to it all the arguments of a send operation.

A communication (send or receive) that uses a persistent request is
initiated by the function :ref:`MPI_Start`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Send_init` :ref:`MPI_Start`
