.. _mpi_file_get_position_shared:


MPI_File_get_position_shared
============================

.. include_body

:ref:`MPI_File_get_position_shared` |mdash| Returns the current position of the
shared file pointer.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_position_shared(MPI_File fh, MPI_Offset *offset)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_POSITION_SHARED(FH, OFFSET, IERROR)
   	INTEGER	FH, IERROR
   	INTEGER(KIND=MPI_OFFSET_KIND)	OFFSET


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_position_shared(fh, offset, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: offset
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``offset``: Offset of the shared file pointer (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_position_shared` returns, in *offset,* the current position
of the shared file pointer in *etype* units relative to the current
displacement and file type.


ERRORS
------

.. include:: ./ERRORS.rst
