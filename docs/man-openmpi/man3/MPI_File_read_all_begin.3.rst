.. _mpi_file_read_all_begin:


MPI_File_read_all_begin
=======================

.. include_body

:ref:`MPI_File_read_all_begin` |mdash| Reads a file starting at the locations
specified by individual file pointers; beginning part of a split
collective routine (nonblocking).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_read_all_begin(MPI_File fh, void *buf,
   	int count, MPI_Datatype datatype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_READ_ALL_BEGIN(FH, BUF, COUNT, DATATYPE, IERROR)
   	<type>	BUF(*)
   	INTEGER	FH, COUNT, DATATYPE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_read_all_begin(fh, buf, count, datatype, ierror)
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

:ref:`MPI_File_read_all_begin` is the beginning part of a split collective
operation that attempts to read from the file associated with *fh* (at
the current individual file pointer position maintained by the system) a
total number of *count* data items having *datatype* type into the
user's buffer *buf.* The data is taken out of those parts of the file
specified by the current view.


NOTES
-----

All the nonblocking collective routines for data access are "split" into
two routines, each with ``_begin`` or ``_end`` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
