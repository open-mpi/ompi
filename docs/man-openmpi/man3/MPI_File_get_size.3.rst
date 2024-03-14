.. _mpi_file_get_size:


MPI_File_get_size
=================

.. include_body

:ref:`MPI_File_get_size` |mdash| Returns the current size of the file.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_size(MPI_File fh, MPI_Offset *size)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_SIZE(FH, SIZE, IERROR)
   	INTEGER	FH, ERROR
   	INTEGER(KIND=MPI_OFFSET_KIND)	SIZE


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_size(fh, size, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``size``: Size of the file in bytes (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_size` returns, in *size* , the current size in bytes of the
file associated with the file handle *fh*. Note that the file size
returned by Solaris may not represent the number of bytes physically
allocated for the file in those cases where all bytes in this file have
not been written at least once.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_File_preallocate`
