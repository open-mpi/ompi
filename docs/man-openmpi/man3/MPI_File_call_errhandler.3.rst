.. _mpi_file_call_errhandler:


MPI_File_call_errhandler
========================

.. include_body

:ref:`MPI_File_call_errhandler` |mdash| Passes the supplied error code to the
error handler assigned to a file


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_call_errhandler(MPI_File fh, int errorcode)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_CALL_ERRHANDLER(FH, ERRORCODE, IERROR)
   	INTEGER	FH, IERRORCODE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_call_errhandler(fh, errorcode, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	INTEGER, INTENT(IN) :: errorcode
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``fh``: file with error handler (handle).
* ``errorcode``: MPI error code (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function invokes the error handler assigned to the file handle *fh*
with the supplied error code *errorcode*. If the error handler was
successfully called, the process is not aborted, and the error handler
returns, this function returns MPI_SUCCESS.

Unlike errors on communicators and windows, the default errorhandler for
files is MPI_ERRORS_RETURN.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_File_create_errhandler`
   * :ref:`MPI_File_set_errhandler`
