.. _mpi_file_seek:


MPI_File_seek
=============

.. include_body

:ref:`MPI_File_seek` - Updates individual file pointers (noncollective).


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_seek(MPI_File fh, MPI_Offset offset,
   	int whence)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_SEEK(FH, OFFSET, WHENCE, IERROR)
   	INTEGER	FH, WHENCE, IERROR
   	INTEGER(KIND=MPI_OFFSET_KIND)	OFFSET


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_seek(fh, offset, whence, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
   	INTEGER, INTENT(IN) :: whence
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: File offset (integer).
* ``whence``: Update mode (integer).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_seek` updates the individual file pointer according to *whence,*
which could have the following possible values:

 o    MPI_SEEK_SET - The pointer is set to *offset.*

 o    MPI_SEEK_CUR - The pointer is set to the current pointer position plus *offset.*

 o    MPI_SEEK_END - The pointer is set to the end of the file plus *offset.*

The *offset* can be negative, which allows seeking backwards. It is
erroneous to seek to a negative position in the file. The end of the
file is defined to be the location of the next elementary data item
immediately after the last accessed data item, even if that location is
a hole.


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the *OFFSET*
argument only for Fortran 90. FORTRAN 77 users may use the non-portable
syntax

::

        INTEGER*MPI_OFFSET_KIND OFFSET

where MPI_OFFSET_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. For MPI I/O function errors, the default error handler is set to
MPI_ERRORS_RETURN. The error handler may be changed with
:ref:`MPI_File_set_errhandler`; the predefined error handler
MPI_ERRORS_ARE_FATAL may be used to make I/O errors fatal. Note that MPI
does not guarantee that an MPI program can continue past an error.
