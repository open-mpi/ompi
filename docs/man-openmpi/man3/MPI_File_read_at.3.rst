.. _mpi_file_read_at:


MPI_File_read_at
================

.. include_body

:ref:`MPI_File_read_at` - Reads a file at an explicitly specified offset
(blocking, noncollective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_read_at(MPI_File fh, MPI_Offset offset,
   	void *buf, int count, MPI_Datatype datatype,
   	MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_READ_AT(FH, OFFSET, BUF, COUNT,
   	DATATYPE, STATUS, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, STATUS(MPI_STATUS_SIZE), IERROR
   	INTEGER(KIND=MPI_OFFSET_KIND)	OFFSET


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_read_at(fh, offset, buf, count, datatype, status, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
   	TYPE(*), DIMENSION(..) :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: File offset (integer).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_at` attempts to read from the file associated with *fh* (at
the *offset* position) a total number of *count* data items having
*datatype* type into the user's buffer *buf.* The *offset* is in *etype*
units relative to the current view. That is, holes are not counted when
locating an offset. The data is taken out of those parts of the file
specified by the current view. :ref:`MPI_File_read_at` stores the number of
*datatype* elements actually read in *status.* All other fields of
*status* are undefined. It is erroneous to call this function if
``MPI_MODE_SEQUENTIAL`` mode was specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
