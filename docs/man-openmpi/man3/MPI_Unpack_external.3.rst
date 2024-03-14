.. _mpi_unpack_external:


MPI_Unpack_external
===================

.. include_body

:ref:`MPI_Unpack_external` |mdash| Reads data from a portable format


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Unpack_external(const char datarep[], const void *inbuf,
   	MPI_Aint insize, MPI_Aint *position,
   	void *outbuf, int outcount,
   	MPI_Datatype datatype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_UNPACK_EXTERNAL(DATAREP, INBUF, INSIZE, POSITION,
   	OUTBUF, OUTCOUNT, DATATYPE, IERROR)

   	INTEGER		OUTCOUNT, DATATYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) INSIZE, POSITION
   	CHARACTER*(*)	DATAREP
   	<type>		INBUF(*), OUTBUF(*)


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Unpack_external(datarep, inbuf, insize, position, outbuf, outcount,
   		datatype, ierror)
   	CHARACTER(LEN=*), INTENT(IN) :: datarep
   	TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
   	TYPE(*), DIMENSION(..) :: outbuf
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: insize
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: position
   	INTEGER, INTENT(IN) :: outcount
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``datarep``: Data Representation (string).
* ``inbuf``: Input buffer start (choice).
* ``insize``: Size of input buffer, in bytes (integer).
* ``outcount``: Number of items to be unpacked (integer).
* ``datatype``: Datatype of each output data item (handle).

INPUT/OUTPUT PARAMETER
----------------------
* ``position``: Current position in buffer, in bytes (integer).

OUTPUT PARAMETERS
-----------------
* ``outbuf``: Output buffer start (choice).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Unpack_external` unpacks data from the external32 format, a universal
data representation defined by the MPI Forum. This format is useful for
exchanging data between MPI implementations, or when writing data to a
file.

The input buffer is a contiguous storage area pointed to by *inbuf*
containing *insize* bytes. The output buffer can be any communication
buffer allowed in :ref:`MPI_Recv`, and is specified by *outbuf*, *outcount*,
and *datatype*.

The input value of *position* is the first position in *inbuf* to be
read for unpacking (measured in bytes, not elements, relative to the
start of the buffer). When the function returns, *position* is
incremented by the size of the packed message, so that it points to the
first location in *inbuf* following the message that was unpacked. This
way it may be used as input to a subsequent call to :ref:`MPI_Unpack_external`.


NOTES
-----

Note the difference between :ref:`MPI_Recv` and :ref:`MPI_Unpack_external`: In
:ref:`MPI_Recv`, the *count* argument specifies the maximum number of items
that can be received. In :ref:`MPI_Unpack_external`, the *outcount* argument
specifies the actual number of items that are to be unpacked. With a
regular receive operation, the incoming message size determines the
number of components that will be received. With :ref:`MPI_Unpack_external`, it
is up to the user to specify how many components to unpack, since the
user may wish to unpack the received message multiple times into various
buffers.

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
datatype: The type-matching rules are relaxed for messages sent with
type MPI_BYTE.

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
   * :ref:`MPI_Pack_external`
   * :ref:`MPI_Pack_external_size`
   * :ref:`MPI_Recv`
   * sscanf(3C)
