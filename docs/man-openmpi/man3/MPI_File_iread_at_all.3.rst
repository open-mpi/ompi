.. _mpi_file_iread_at_all:


MPI_File_iread_at_all
=====================

.. include_body

:ref:`MPI_File_iread_at_all` |mdash| Reads a file at an explicitly specified
offset (nonblocking, collective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_iread_at_all(MPI_File fh, MPI_Offset offset,
   	void *buf, int count, MPI_Datatype datatype,
   	MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_IREAD_AT_ALL(FH, OFFSET, BUF, COUNT, DATATYPE, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, REQUEST, IERROR
   	INTEGER(KIND=MPI_OFFSET_KIND)	OFFSET


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_iread_at_all(fh, offset, buf, count, datatype, request, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
   	TYPE(*), DIMENSION(..) :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: File offset (integer).
* ``count``: Number of elements in the buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of the buffer (choice).
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iread_at_all` is the nonblocking version of
:ref:`MPI_File_read_at_all`.

:ref:`MPI_File_iread_at_all` is a nonblocking routine that attempts to read
from the file associated with *fh* at the *offset* position a total
number of *count* data items having *datatype* type into the user's
buffer *buf.* The *offset* is in etype units relative to the current
view. That is, holes are not counted when locating an offset. The data
is taken out of those parts of the file specified by the current view.
:ref:`MPI_File_iread_at_all` stores the number of *datatype* elements actually
read in *status.* All other fields of *status* are undefined.


ERRORS
------

.. include:: ./ERRORS.rst
