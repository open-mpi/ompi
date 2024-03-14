.. _mpi_file_read_ordered:


MPI_File_read_ordered
=====================

.. include_body

:ref:`MPI_File_read_ordered` |mdash| Reads a file at a location specified by a
shared file pointer (blocking, collective).


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_read_ordered(MPI_File fh, void *buf,
   	int count, MPI_Datatype datatype,
   	MPI_Status *status)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_READ_ORDERED(FH, BUF, COUNT, DATATYPE,
   	STATUS, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, STATUS(MPI_STATUS_SIZE), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_read_ordered(fh, buf, count, datatype, status, ierror)
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
* ``status``: Status object (Status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_ordered` is a collective routine. This routine must be
called by all processes in the communicator group associated with the
file handle *fh.* Each process may pass different argument values for
the *datatype* and *count* arguments. Each process attempts to read,
from the file associated with *fh,* a total number of *count* data items
having *datatype* type into the user's buffer *buf.* For each process,
the location in the file at which data is read is the position at which
the shared file pointer would be after all processes whose ranks within
the group are less than that of this process had read their data.
:ref:`MPI_File_read_ordered` returns the actual number of *datatype* elements
read in *status.* The shared file pointer is updated by the amounts of
data requested by all processes of the group.


ERRORS
------

.. include:: ./ERRORS.rst
