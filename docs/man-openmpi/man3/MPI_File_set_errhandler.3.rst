.. _mpi_file_set_errhandler:


MPI_File_set_errhandler
=======================

.. include_body

:ref:`MPI_File_set_errhandler` |mdash| Sets the error handler for a file.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_set_errhandler(MPI_File file, MPI_Errhandler
   	errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_SET_ERRHANDLER(FILE, ERRHANDLER, IERROR)
   	INTEGER	FILE, ERRHANDLER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_set_errhandler(file, errhandler, ierror)
   	TYPE(MPI_File), INTENT(IN) :: file
   	TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``file``: File (handle).

INPUT PARAMETER
---------------
* ``errhandler``: New error handler for file (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Attaches a new error handler to a file. The error handler must be either
a predefined error handler or an error handler created by a call to
:ref:`MPI_File_create_errhandler`.


ERRORS
------

.. include:: ./ERRORS.rst
