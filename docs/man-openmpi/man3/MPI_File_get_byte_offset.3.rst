.. _mpi_file_get_byte_offset:


MPI_File_get_byte_offset
========================

.. include_body

:ref:`MPI_File_get_byte_offset` |mdash| Converts a view-relative offset into an
absolute byte position.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset,
   	MPI_Offset *disp)


Fortran Syntax
^^^^^^^^^^^^^^

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
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_byte_offset` converts an offset specified for the current
view to its corresponding displacement value, or absolute byte position,
from the beginning of the file. The absolute byte position of *offset*
relative to the current view of *fh* is returned in *disp*.


ERRORS
------

.. include:: ./ERRORS.rst
