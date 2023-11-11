.. _mpi_file_iwrite_at:


MPI_File_iwrite_at
==================

.. include_body

:ref:`MPI_File_iwrite_at` - Writes a file at an explicitly specified offset
(nonblocking, noncollective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset,
   	const void *buf, int count, MPI_Datatype datatype, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_IWRITE_AT(FH, OFFSET, BUF, COUNT, DATATYPE, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, REQUEST, IERROR
   	INTEGER(KIND=MPI_OFFSET_KIND)	OFFSET


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_iwrite_at(fh, offset, buf, count, datatype, request, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``offset``: File offset (integer).
* ``buf``: Initial address of buffer (choice).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iwrite_at` is a nonblocking version of :ref:`MPI_File_write_at`. It
attempts to write into the file associated with *fh* (at the *offset*
position) a total number of *count* data items having *datatype* type
from the user's buffer *buf.* The offset is in *etype* units relative to
the current view. That is, holes are not counted when locating an
offset. The data is written into those parts of the file specified by
the current view. :ref:`MPI_File_iwrite_at` stores the number of *datatype*
elements actually written in *status.* All other fields of *status* are
undefined. The request structure can be passed to :ref:`MPI_Wait` or :ref:`MPI_Test`,
which will return a status with the number of bytes actually accessed.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was open.


ERRORS
------

.. include:: ./ERRORS.rst
