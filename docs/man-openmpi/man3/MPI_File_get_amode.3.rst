.. _mpi_file_get_amode:


MPI_File_get_amode
==================

.. include_body

:ref:`MPI_File_get_amode` |mdash| Returns access mode associated with an open
file.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_amode(MPI_File fh, int *amode)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_AMODE(FH, AMODE, IERROR)
   	INTEGER	FH, AMODE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_amode(fh, amode, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER, INTENT(OUT) :: amode
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``amode``: File access mode used to open the file (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_amode` returns, in *amode,* the access mode associated with
the open file *fh.*


ERRORS
------

.. include:: ./ERRORS.rst
