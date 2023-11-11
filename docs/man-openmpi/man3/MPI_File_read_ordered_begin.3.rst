.. _mpi_file_read_ordered_begin:


MPI_File_read_ordered_begin
===========================

.. include_body

:ref:`MPI_File_read_ordered_begin` - Reads a file at a location specified
by a shared file pointer; beginning part of a split collective routine
(nonblocking).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_read_ordered_begin(MPI_File fh, void *buf,
   	int count, MPI_Datatype datatype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_READ_ORDERED_BEGIN(FH, BUF, COUNT, DATATYPE, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_read_ordered_begin(fh, buf, count, datatype, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
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
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_ordered_begin` is the beginning part of a split collective,
nonblocking routine that must be called by all processes in the
communicator group associated with the file handle *fh.* Each process
may pass different argument values for the *datatype* and *count*
arguments. Each process attempts to read, from the file associated with
*fh,* a total number of *count* data items having *datatype* type into
the user's buffer *buf.* For each process, the location in the file at
which data is read is the position at which the shared file pointer
would be after all processes whose ranks within the group are less than
that of this process had read their data.


NOTES
-----

All the nonblocking collective routines for data access are "split" into
two routines, each with ``_begin`` or ``_end`` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
