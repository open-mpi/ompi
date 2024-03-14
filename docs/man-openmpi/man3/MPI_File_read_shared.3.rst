.. _mpi_file_read_shared:


MPI_File_read_shared
====================

.. include_body

:ref:`MPI_File_read_shared` |mdash| Reads a file using the shared file pointer
(blocking, noncollective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_read_shared(MPI_File fh, void *buf, int count,
   	MPI_Datatype datatype, MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_READ_SHARED(FH, BUF, COUNT, DATATYPE, STATUS,
   	IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE,STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_read_shared(fh, buf, count, datatype, status, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(*), DIMENSION(..) :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Status) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``count``: Number of elements in buffer (integer)
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_shared` is a blocking routine that uses the shared file
pointer to read files. The order of serialization is not deterministic
for this noncollective routine.


ERRORS
------

.. include:: ./ERRORS.rst
