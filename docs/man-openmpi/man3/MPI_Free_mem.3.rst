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

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Alloc_mem`
