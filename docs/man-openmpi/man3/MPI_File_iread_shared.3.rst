.. _mpi_file_iread_shared:


MPI_File_iread_shared
=====================

.. include_body

:ref:`MPI_File_iread_shared` |mdash| Reads a file using the shared file pointer
(nonblocking, noncollective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_iread_shared(MPI_File fh, void *buf, int count,
   	MPI_Datatype datatype, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_IREAD_SHARED(FH, BUF, COUNT, DATATYPE, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_iread_shared(fh, buf, count, datatype, request, ierror)
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
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iread_shared` is a nonblocking version of the
:ref:`MPI_File_read_shared` interface. It uses the shared file pointer to read
files. The order of serialization among the processors is not
deterministic for this noncollective routine, so you need to use other
methods of synchronization to impose a particular order among
processors.


ERRORS
------

.. include:: ./ERRORS.rst
