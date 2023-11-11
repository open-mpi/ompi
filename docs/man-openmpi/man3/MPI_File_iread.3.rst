.. _mpi_file_iread:


MPI_File_iread
==============

.. include_body

:ref:`MPI_File_iread` |mdash| Reads a file starting at the location specified by
the individual file pointer (nonblocking, noncollective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_iread(MPI_File fh, void  *buf, int  count,
   	MPI_Datatype  datatype, MPI_Request  *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_IREAD(FH, BUF, COUNT, DATATYPE, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_iread(fh, buf, count, datatype, request, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``count``: Number of elements in the buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iread` is a nonblocking version of :ref:`MPI_File_read`. It attempts to
read from the file associated with *fh* at the current individual file
pointer position maintained by the system in which a total number of
*count* data items having *datatype* type are read into the user's
buffer *buf.* The data is taken out of those parts of the file specified
by the current view. :ref:`MPI_File_iread` stores the number of data-type
elements actually read in *status.* All other fields of *status* are
undefined. It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL``
mode was specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
