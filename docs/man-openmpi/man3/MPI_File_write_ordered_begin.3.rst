.. _mpi_file_write_ordered_begin:

MPI_File_write_ordered_begin
============================

.. include_body

:ref:`MPI_File_write_ordered_begin` - Writes a file at a location specified
by a shared file pointer; beginning part of a split collective routine
(nonblocking).

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_write_ordered_begin(MPI_File fh, const void *buf,
       int count, MPI_Datatype datatype)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_FILE_WRITE_ORDERED_BEGIN(FH, BUF, COUNT, DATATYPE, IERROR)
       <type>  BUF(*)
       INTEGER FH, COUNT, DATATYPE, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_File_write_ordered_begin(fh, buf, count, datatype, ierror)
       TYPE(MPI_File), INTENT(IN) :: fh
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
       INTEGER, INTENT(IN) :: count
       TYPE(MPI_Datatype), INTENT(IN) :: datatype
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT/OUTPUT PARAMETER
----------------------

* ``fh`` : File handle (handle).

INPUT PARAMETERS
----------------

* ``buf`` : Initial address of buffer (choice).
* ``count`` : Number of elements in buffer (integer).
* ``datatype`` : Data type of each buffer element (handle).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write_ordered_begin` is the beginning part of a split
collective, nonblocking routine that must be called by all processes in
the communicator group associated with the file handle ``fh``. Each
process may pass different argument values for the ``datatype`` and
``count`` arguments. After all processes of the group have issued their
respective calls, each process attempts to write, into the file
associated with ``fh``, a total number of ``count`` data items having
``datatype`` type contained in the user's buffer ``buf``. For each
process, the location in the file at which data is written is the
position at which the shared file pointer would be after all processes
whose ranks within the group are less than that of this process had
written their data.

NOTES
-----

All the nonblocking collective routines for data access are "split" into
two routines, each with ``_begin`` or ``_end`` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.

ERRORS
------

.. include:: ./ERRORS.rst
