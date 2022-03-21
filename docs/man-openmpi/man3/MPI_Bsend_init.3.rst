.. _mpi_bsend_init:

MPI_Bsend_init
==============

.. include_body

:ref:`MPI_Bsend_init` - Builds a handle for a buffered send.

Syntax
------

C Syntax
^^^^^^^^

.. code:: C

   #include <mpi.h>

   int MPI_Bsend_init(const void *buf, int count, MPI_Datatype datatype,
       int dest, int tag, MPI_Comm comm, MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_BSEND_INIT(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
       <type>  BUF(*)
       INTEGER COUNT, DATATYPE, DEST, TAG,
       INTEGER COMM, REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: Fortran

   USE mpi_f08

   MPI_Bsend_init(buf, count, datatype, dest, tag, comm, request, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
       INTEGER, INTENT(IN) :: count, dest, tag
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Request), INTENT(OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  buf : Initial address of send buffer (choice).
-  count : Number of elements sent (integer).
-  datatype : Type of each element (handle).
-  dest : Rank of destination (integer).
-  tag : Message tag (integer).
-  comm : Communicator (handle).

Output Parameters
-----------------

-  request : Communication request (handle).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

Creates a persistent communication request for a buffered mode send, and
binds to it all the arguments of a send operation.

A communication (send or receive) that uses a persistent request is
initiated by the function :ref:`MPI_Start`.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso:: :ref:`MPI_Send_init` :ref:`MPI_Start`
