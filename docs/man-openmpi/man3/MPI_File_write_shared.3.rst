.. _mpi_file_write_shared:

MPI_File_write_shared
=====================

.. include_body

:ref:`MPI_File_write_shared` |mdash| Writes a file using the shared file pointer
(blocking, noncollective).

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_write_shared(MPI_File fh, const void *buf, int count,
       MPI_Datatype datatype, MPI_Status *status)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_FILE_WRITE_SHARED(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)
       <type>  BUF(*)
         INTEGER   FH, COUNT, DATATYPE, STATUS(MPI_STATUS_SIZE), IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_File_write_shared(fh, buf, count, datatype, status, ierror)
       TYPE(MPI_File), INTENT(IN) :: fh
       TYPE(*), DIMENSION(..), INTENT(IN) :: buf
       INTEGER, INTENT(IN) :: count
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       TYPE(MPI_Status) :: status
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT/OUTPUT PARAMETER
----------------------

* ``fh`` : File handle (handle).

INPUT PARAMETERS
----------------

* ``buf`` : Initial address of buffer (choice).
* ``count`` : Number of elements in buffer (integer).
* ``datatype`` : Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------

* ``status`` : Status object (status).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write_shared` is a blocking routine that uses the shared
file pointer to write files. The order of serialization is not
deterministic for this noncollective routine.

ERRORS
------

.. include:: ./ERRORS.rst
