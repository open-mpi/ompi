.. _mpi_pack:


MPI_Pack
========

.. include_body

:ref:`MPI_Pack` |mdash| Packs data of a given datatype into contiguous memory.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Pack(const void *inbuf, int incount, MPI_Datatype datatype,
   	void *outbuf, int outsize, int *position, MPI_Comm comm)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PACK(INBUF, INCOUNT, DATATYPE, OUTBUF,OUTSIZE, POSITION,
   		COMM, IERROR)
   	<type>	INBUF(*), OUTBUF(*)
   	INTEGER	INCOUNT, DATATYPE, OUTSIZE, POSITION, COMM, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Pack(inbuf, incount, datatype, outbuf, outsize, position, comm, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
   	TYPE(*), DIMENSION(..) :: outbuf
   	INTEGER, INTENT(IN) :: incount, outsize
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, INTENT(INOUT) :: position
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``inbuf``: Input buffer start (choice).
* ``incount``: Number of input data items (integer).
* ``datatype``: Datatype of each input data item (handle).
* ``outsize``: Output buffer size, in bytes (integer).
* ``comm``: Communicator for packed message (handle).

INPUT/OUTPUT PARAMETER
----------------------
* ``position``: Current position in buffer, in bytes (integer).

OUTPUT PARAMETERS
-----------------
* ``outbuf``: Output buffer start (choice).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Packs the message in the send buffer specified by *inbuf*, *incount*,
*datatype* into the buffer space specified by *outbuf* and *outsize*.
The input buffer can be any communication buffer allowed in :ref:`MPI_Send`.
The output buffer is a contiguous storage area containing *outsize*
bytes, starting at the address *outbuf* (length is counted in bytes, not
elements, as if it were a communication buffer for a message of type
MPI_Packed).

The input value of *position* is the first location in the output buffer
to be used for packing. *position* is incremented by the size of the
packed message, and the output value of *position* is the first location
in the output buffer following the locations occupied by the packed
message. The *comm* argument is the communicator that will be
subsequently used for sending the packed message.

**Example:** An example using :ref:`MPI_Pack`:

.. code-block:: c

   int myrank, position, i, j, a[2];
   char buff[1000];

   // ...

   MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
   if (myrank == 0) {
     /* SENDER CODE */
     position = 0;
     MPI_Pack(&i, 1, MPI_INT, buff, 1000, &position, MPI_COMM_WORLD);
     MPI_Pack(&j, 1, MPI_INT, buff, 1000, &position, MPI_COMM_WORLD);
     MPI_Send(buff, position, MPI_PACKED, 1, 0, MPI_COMM_WORLD);
   } else {
     /* RECEIVER CODE */
     MPI_Recv(a, 2, MPI_INT, 0, 0, MPI_COMM_WORLD);
   }


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Unpack`
   * :ref:`MPI_Pack_size`
