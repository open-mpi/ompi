.. _mpi_pack:


MPI_Pack
========

.. include_body

:ref:`MPI_Pack` |mdash| Packs data of a given datatype into contiguous memory.

.. The following file was automatically generated
.. include:: ./bindings/mpi_pack.rst

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
