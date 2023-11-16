.. _mpi_file_get_atomicity:


MPI_File_get_atomicity
======================

.. include_body

:ref:`MPI_File_get_atomicity` |mdash| Returns current consistency semantics for
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
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_atomicity` returns the current consistency semantics for
data access operations on the set of file handles created by one
collective :ref:`MPI_File_open`. If *flag* is *true,* atomic mode is currently
enabled; if *flag* is *false,* nonatomic mode is currently enabled.


ERRORS
------

.. include:: ./ERRORS.rst
