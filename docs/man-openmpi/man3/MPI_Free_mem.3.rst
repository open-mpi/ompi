.. _mpi_free_mem:

MPI_Free_mem
============

.. include_body

:ref:`MPI_Free_mem` |mdash| Frees memory that has been allocated using :ref:`MPI_Alloc_mem`.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Free_mem(void *base)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_FREE_MEM(BASE, IERROR)
       <type> BASE(*)
       INTEGER IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Free_mem(base, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: base
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETER
---------------

* ``base`` : Initial address of memory segment allocated by :ref:`MPI_Alloc_mem`
   (choice).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Free_mem` frees memory that has been allocated by :ref:`MPI_Alloc_mem`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Alloc_mem`
