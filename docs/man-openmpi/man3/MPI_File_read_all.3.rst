.. _mpi_file_read_all:


MPI_File_read_all
=================

.. include_body

:ref:`MPI_File_read_all` - Reads a file starting at the locations specified
by individual file pointers (blocking, collective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_read_all(MPI_File fh, void *buf,
   	int count, MPI_Datatype datatype, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_READ_ALL(FH, BUF, COUNT,
   	DATATYPE, STATUS, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, STATUS(MPI_STATUS_SIZE),IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_read_all(fh, buf, count, datatype, status, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(*), DIMENSION(..) :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_all` is a collective routine that attempts to read from the
file associated with *fh* (at the current individual file pointer
position maintained by the system) a total number of *count* data items
having *datatype* type into the user's buffer *buf.* The data is taken
out of those parts of the file specified by the current view.
:ref:`MPI_File_read_all` stores the number of data-type elements actually read
in *status.* All other fields of *status* are undefined.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
