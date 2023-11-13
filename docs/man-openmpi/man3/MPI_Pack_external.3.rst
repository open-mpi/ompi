.. _mpi_pack_external:


MPI_Pack_external
=================

.. include_body

:ref:`MPI_Pack_external` |mdash| Writes data to a portable format


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Pack_external(const char *datarep, const void *inbuf,
   	int incount, MPI_Datatype datatype,
   	void *outbuf, MPI_Aint outsize,
   	MPI_Aint *position)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PACK_EXTERNAL(DATAREP, INBUF, INCOUNT, DATATYPE,
   	OUTBUF, OUTSIZE, POSITION, IERROR)

   	INTEGER		INCOUNT, DATATYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) OUTSIZE, POSITION
   	CHARACTER*(*)	DATAREP
   	<type>		INBUF(*), OUTBUF(*)


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Pack_external(datarep, inbuf, incount, datatype, outbuf, outsize,
   		position, ierror)
   	CHARACTER(LEN=*), INTENT(IN) :: datarep
   	TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
   	TYPE(*), DIMENSION(..) :: outbuf
   	INTEGER, INTENT(IN) :: incount
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: outsize
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: position
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``datarep``: Data representation (string).
* ``inbuf``: Input buffer start (choice).
* ``incount``: Number of input data items (integer).
* ``datatype``: Datatype of each input data item (handle).
* ``outsize``: Output buffer size, in bytes (integer).

INPUT/OUTPUT PARAMETER
----------------------
* ``position``: Current position in buffer, in bytes (integer).

OUTPUT PARAMETERS
-----------------
* ``outbuf``: Output buffer start (choice).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Pack_external` packs data into the external32 format, a universal
data representation defined by the MPI Forum. This format is useful for
exchanging data between MPI implementations, or when writing data to a
file.

The input buffer is specified by *inbuf*, *incount* and *datatype*, and
may be any communication buffer allowed in :ref:`MPI_Send`. The output buffer
*outbuf* must be a contiguous storage area containing *outsize* bytes.

The input value of *position* is the first position in *outbuf* to be
used for packing (measured in bytes, not elements, relative to the start
of the buffer). When the function returns, *position* is incremented by
the size of the packed message, so that it points to the first location
in *outbuf* following the packed message. This way it may be used as
input to a subsequent call to :ref:`MPI_Pack_external`.

**Example:** An example using :ref:`MPI_Pack_external`:

.. code-block:: c

   	int position, i;
   	double msg[5];
   	char buf[1000];

   	...

   	MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
   	if (myrank == 0) {	/* SENDER CODE */
   		position = 0;
   		i = 5; /* number of doubles in msg[] */
   		MPI_Pack_external("external32", &i, 1, MPI_INT,
   		    buf, 1000, &position);
   		MPI_Pack_external("external32", &msg, i, MPI_DOUBLE,
   		    buf, 1000, &position);
   		MPI_Send(buf, position, MPI_BYTE, 1, 0,
   		    MPI_COMM_WORLD);
   	} else {		/* RECEIVER CODE */
   		MPI_Recv(buf, 1, MPI_BYTE, 0, 0, MPI_COMM_WORLD,
   		    MPI_STATUS_IGNORE);
   		MPI_Unpack_external("external32", buf, 1000,
   		    MPI_INT, &i, 1, &position);
   		MPI_Unpack_external("external32", buf, 1000,
   		    MPI_DOUBLE, &msg, i, &position);
   	}


NOTES
-----

The *datarep* argument specifies the data format. The only valid value
in the current version of MPI is "external32". The argument is provided
for future extensibility.

To understand the behavior of pack and unpack, it is convenient to think
of the data part of a message as being the sequence obtained by
concatenating the successive values sent in that message. The pack
operation stores this sequence in the buffer space, as if sending the
message to that buffer. The unpack operation retrieves this sequence
from buffer space, as if receiving a message from that buffer. (It is
helpful to think of internal Fortran files or sscanf in C for a similar
function.)

Several messages can be successively packed into one packing unit. This
is effected by several successive related calls to :ref:`MPI_Pack_external`,
where the first call provides *position*\ =0, and each successive call
inputs the value of *position* that was output by the previous call,
along with the same values for *outbuf* and *outcount*. This packing
unit now contains the equivalent information that would have been stored
in a message by one send call with a send buffer that is the
"concatenation" of the individual send buffers.

A packing unit can be sent using type MPI_BYTE. Any point-to-point or
collective communication function can be used to move the sequence of
bytes that forms the packing unit from one process to another. This
packing unit can now be received using any receive operation, with any
datatype. (The type-matching rules are relaxed for messages sent with
type MPI_BYTE.)

A packing unit can be unpacked into several successive messages. This is
effected by several successive related calls to :ref:`MPI_Unpack_external`,
where the first call provides *position*\ =0, and each successive call
inputs the value of position that was output by the previous call, and
the same values for *inbuf* and *insize*.

The concatenation of two packing units is not necessarily a packing
unit; nor is a substring of a packing unit necessarily a packing unit.
Thus, one cannot concatenate two packing units and then unpack the
result as one packing unit; nor can one unpack a substring of a packing
unit as a separate packing unit. Each packing unit that was created by a
related sequence of pack calls must be unpacked as a unit by a sequence
of related unpack calls.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pack_external_size`
   * :ref:`MPI_Send`
   * :ref:`MPI_Unpack_external`
   * sscanf(3C)
