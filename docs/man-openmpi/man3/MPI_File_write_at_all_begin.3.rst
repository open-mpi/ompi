.. _mpi_file_write_at_all_begin:


MPI_File_write_at_all_begin
===========================

.. include_body

:ref:`MPI_File_write_at_all_begin` - Writes a file at explicitly specified
offsets; beginning part of a split collective routine (nonblocking).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset,
   	const void *buf, int count, MPI_Datatype datatype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_WRITE_AT_ALL_BEGIN(FH, OFFSET, BUF, COUNT, DATATYPE, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, IERROR
   	INTEGER(KIND=MPI_OFFSET_KIND)	OFFSET


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_write_at_all_begin(fh, offset, buf, count, datatype, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``offset``: File offset (handle).
* ``buf``: Initial address of buffer (choice).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write_at_all_begin` is the beginning part of a split collective,
that is, a nonblocking routine that attempts to write into the file
associated with *fh* (at the *offset* position) a total number of
*count* data items having *datatype* type from the user's buffer *buf.*
The offset is in etype units relative to the current view. That is,
holes are not counted when locating an offset. The data is written into
those parts of the file specified by the current view.


NOTES
-----

All the nonblocking collective routines for data access are "split" into
two routines, each with ``_begin`` or ``_end`` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
