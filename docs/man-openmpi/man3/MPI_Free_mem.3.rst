.. _mpi_free_mem:

MPI_Free_mem
============

.. include_body

:ref:`MPI_Free_mem` - Frees memory that has been allocated using :ref:`MPI_Alloc_mem`.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Free_mem(void *base)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_FREE_MEM(BASE, IERROR)
       <type> BASE(*)
       INTEGER IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Free_mem(base, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: base
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameter
---------------

-  base : Initial address of memory segment allocated by :ref:`MPI_Alloc_mem`
   (choice).

Output Parameter
----------------

-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Free_mem` frees memory that has been allocated by :ref:`MPI_Alloc_mem`.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso:: :ref:`MPI_Alloc_mem`
