.. _mpi_file_get_errhandler:


MPI_File_get_errhandler
=======================

.. include_body

:ref:`MPI_File_get_errhandler` |mdash| Gets the error handler for a file.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_errhandler(MPI_File file, MPI_Errhandler
   	*errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_ERRHANDLER(FILE, ERRHANDLER, IERROR)
   	INTEGER	FILE, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_errhandler(file, errhandler, ierror)
   	TYPE(MPI_File), INTENT(IN) :: file
   	TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``file``: File (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: MPI error handler currently associated with file (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns in *errhandler* (a handle to) the error handler that is
currently associated with file *file*.


ERRORS
------

.. include:: ./ERRORS.rst
