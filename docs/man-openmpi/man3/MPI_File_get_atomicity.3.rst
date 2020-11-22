.. _mpi_file_get_atomicity:


MPI_File_get_atomicity
======================

.. include_body

:ref:`MPI_File_get_atomicity` - Returns current consistency semantics for
data-access operations.


SYNTAX
------



C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_File_get_atomicity(MPI_File fh, int *flag)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_FILE_GET_ATOMICITY(FH, FLAG, IERROR)
   	INTEGER	FH, IERROR
   	LOGICAL	FLAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_File_get_atomicity(fh, flag, ierror)
   	TYPE(MPI_File), INTENT(IN) :: fh
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETER
----------------
* ``flag``: true if atomic mode is enabled, false if nonatomic mode is enabled (boolean).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_atomicity` returns the current consistency semantics for
data access operations on the set of file handles created by one
collective :ref:`MPI_File_open`. If *flag* is *true,* atomic mode is currently
enabled; if *flag* is *false,* nonatomic mode is currently enabled.


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
