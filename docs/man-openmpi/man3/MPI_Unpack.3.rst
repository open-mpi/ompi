.. _mpi_unpack:


MPI_Unpack
==========

.. include_body

:ref:`MPI_Unpack` |mdash| Unpacks a datatype into contiguous memory.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Unpack(const void *inbuf, int insize, int *position,
   	void *outbuf, int outcount, MPI_Datatype datatype,
   	MPI_Comm comm)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_UNPACK(INBUF, INSIZE, POSITION, OUTBUF, OUTCOUNT,
   	DATATYPE, COMM, IERROR)
   	<type>	INBUF(*), OUTBUF(*)
   	INTEGER	INSIZE, POSITION, OUTCOUNT, DATATYPE,
   		COMM, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Unpack(inbuf, insize, position, outbuf, outcount, datatype, comm,
   		ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
   	TYPE(*), DIMENSION(..) :: outbuf
   	INTEGER, INTENT(IN) :: insize, outcount
   	INTEGER, INTENT(INOUT) :: position
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``inbuf``: Input buffer start (choice).
* ``insize``: Size of input buffer, in bytes (integer).
* ``outcount``: Number of items to be unpacked (integer).
* ``datatype``: Datatype of each output data item (handle).
* ``comm``: Communicator for packed message (handle).

INPUT/OUTPUT PARAMETER
----------------------
* ``position``: Current position in bytes (integer).

OUTPUT PARAMETERS
-----------------
* ``outbuf``: Output buffer start (choice).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Unpacks a message into the receive buffer specified by outbuf, outcount,
datatype from the buffer space specified by inbuf and insize. The output
buffer can be any communication buffer allowed in :ref:`MPI_Recv`. The input
buffer is a contiguous storage area containing insize bytes, starting at
address inbuf. The input value of position is the first location in the
input buffer occupied by the packed message. *position* is incremented
by the size of the packed message, so that the output value of position
is the first location in the input buffer after the locations occupied
by the message that was unpacked. *comm* is the communicator used to
receive the packed message.


NOTES
-----

Note the difference between :ref:`MPI_Recv` and :ref:`MPI_Unpack`: In :ref:`MPI_Recv`, the
*count* argument specifies the maximum number of items that can be
received. The actual number of items received is determined by the
length of the incoming message. In :ref:`MPI_Unpack`, the count argument
specifies the actual number of items that are to be unpacked; the "size"
of the corresponding message is the increment in position. The reason
for this change is that the "incoming message size" is not predetermined
since the user decides how much to unpack; nor is it easy to determine
the "message size" from the number of items to be unpacked.

To understand the behavior of pack and unpack, it is convenient to think
of the data part of a message as being the sequence obtained by
concatenating the successive values sent in that message. The pack
operation stores this sequence in the buffer space, as if sending the
message to that buffer. The unpack operation retrieves this sequence
from buffer space, as if receiving a message from that buffer. (It is
helpful to think of internal Fortran files or sscanf in C for a similar
function.)

Several messages can be successively packed into one packing unit. This
is effected by several successive related calls to :ref:`MPI_Pack`, where the
first call provides position = 0, and each successive call inputs the
value of position that was output by the previous call, and the same
values for outbuf, outcount, and comm. This packing unit now contains
the equivalent information that would have been stored in a message by
one send call with a send buffer that is the "concatenation" of the
individual send buffers.

A packing unit can be sent using type MPI_Packed. Any point-to-point or
collective communication function can be used to move the sequence of
bytes that forms the packing unit from one process to another. This
packing unit can now be received using any receive operation, with any
datatype: The type-matching rules are relaxed for messages sent with
type MPI_Packed.

A message sent with any type (including MPI_Packed) can be received
using the type MPI_Packed. Such a message can then be unpacked by calls
to :ref:`MPI_Unpack`.

A packing unit (or a message created by a regular, "typed" send) can be
unpacked into several successive messages. This is effected by several
successive related calls to :ref:`MPI_Unpack`, where the first call provides
position = 0, and each successive call inputs the value of position that
was output by the previous call, and the same values for inbuf, insize,
and comm.

The concatenation of two packing units is not necessarily a packing
unit; nor is a substring of a packing unit necessarily a packing unit.
Thus, one cannot concatenate two packing units and then unpack the
result as one packing unit; nor can one unpack a substring of a packing
unit as a separate packing unit. Each packing unit that was created by a
related sequence of pack calls or by a regular send must be unpacked as
a unit, by a sequence of related unpack calls.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pack`
   * :ref:`MPI_Pack_size`
