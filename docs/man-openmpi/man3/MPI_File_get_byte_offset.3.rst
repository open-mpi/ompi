.. _mpi_file_get_byte_offset:


MPI_File_get_byte_offset
========================

.. include_body

:ref:`MPI_File_get_byte_offset` - Converts a view-relative offset into an
absolute byte position.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset,
   	MPI_Offset *disp)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_BYTE_OFFSET(FH, OFFSET, DISP, IERROR)
   	INTEGER	FH, IERROR
   	INTEGER(KIND=MPI_OFFSET_KIND)	OFFSET, DISP


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_byte_offset(fh, offset, disp, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: disp
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: Offset (integer).

OUTPUT PARAMETERS
-----------------
* ``disp``: Absolute byte position of offset (integer).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_byte_offset` converts an offset specified for the current
view to its corresponding displacement value, or absolute byte position,
from the beginning of the file. The absolute byte position of *offset*
relative to the current view of *fh* is returned in *disp*.


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the *OFFSET* and
*DISP* arguments only for Fortran 90. Sun FORTRAN 77 users may use the
non-portable syntax

::

        INTEGER*MPI_OFFSET_KIND OFFSET
   or
        INTEGER*MPI_OFFSET_KIND DISP

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
