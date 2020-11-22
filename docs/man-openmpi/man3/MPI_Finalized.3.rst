.. _mpi_finalized:

MPI_Finalized
=============

.. include_body

:ref:`MPI_Finalized` - Checks whether MPI has been finalized

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Finalized(int *flag)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_FINALIZED(FLAG, IERROR)
       LOGICAL FLAG
       INTEGER IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Finalized(flag, ierror)
       LOGICAL, INTENT(OUT) :: flag
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Output Parameter
----------------

-  flag : True if MPI was finalized, and false otherwise (logical).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

This routine may be used to determine whether MPI has been finalized. It
is one of a small number of routines that may be called before MPI is
initialized and after MPI has been finalized (:ref:`MPI_Initialized` is
another).

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with :ref:`MPI_Comm_set_errhandler`;
the predefined error handler MPI_ERRORS_RETURN may be used to cause
error values to be returned. Note that MPI does not guarantee that an
MPI program can continue past an error.


.. seealso:: :ref:`MPI_Init`
