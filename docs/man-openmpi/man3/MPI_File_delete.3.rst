.. _mpi_file_delete:


MPI_File_delete
===============

.. include_body

:ref:`MPI_File_delete` - Deletes a file.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_delete(const char *filename, MPI_Info info)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_DELETE(FILENAME, INFO, IERROR)
   	CHARACTER*(*)	FILENAME
   	INTEGER	INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_delete(filename, info, ierror)
   	CHARACTER(LEN=*), INTENT(IN) :: filename
   	TYPE(MPI_Info), INTENT(IN) :: info
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``filename``: Name of file to delete (string).
* ``info``: Info object (handle).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_delete` deletes the file identified by the file name *filename*,
provided it is not currently open by any process. It is an error to
delete the file with :ref:`MPI_File_delete` if some process has it open, but
:ref:`MPI_File_delete` does not check this. If the file does not exist,
:ref:`MPI_File_delete` returns an error in the class MPI_ERR_NO_SUCH_FILE.


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
