.. _mpi_file_iwrite_all:


MPI_File_iwrite_all
===================

.. include_body

:ref:`MPI_File_iwrite_all` - Writes a file starting at the location
specified by the individual file pointer (nonblocking, collective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_iwrite_all(MPI_File fh, const void *buf, int count,
   	MPI_Datatype datatype, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_IWRITE_ALL(FH, BUF, COUNT, DATATYPE, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_iwrite_all(fh, buf, count, datatype, request, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(*), DIMENSION(..) :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``buf``: Initial address of buffer (choice).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETER
----------------
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iwrite_all` is a nonblocking version of the :ref:`MPI_File_write_all`
interface. It attempts to write into the file associated with *fh* (at
the current individual file pointer position maintained by the system) a
total number of *count* data items having *datatype* type from the
user's buffer *buf.* The data is written into those parts of the file
specified by the current view. :ref:`MPI_File_iwrite_all` stores the number of
*datatype* elements actually written in *status.* All other fields of
*status* are undefined.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was open.


ERRORS
------

.. include:: ./ERRORS.rst
