.. _mpi_alloc_mem:


MPI_Alloc_mem
=============

.. include_body

:ref:`MPI_Alloc_mem` - Allocates a specified memory segment.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ALLOC_MEM(SIZE, INFO, BASEPTR, IERROR)
   	INTEGER INFO, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) SIZE, BASEPTR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Alloc_mem(size, info, baseptr, ierror)
   	USE, INTRINSIC :: ISO_C_BINDING, ONLY
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(C_PTR), INTENT(OUT) :: baseptr
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``size``: Size of memory segment in bytes (nonnegative integer).
* ``info``: Info argument (handle).

OUTPUT PARAMETERS
-----------------
* ``baseptr``: Pointer to beginning of memory segment allocated.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Alloc_mem` allocates *size* bytes of memory. The starting address of
this memory is returned in the variable *baseptr*.


C NOTES
-------

The parameter *baseptr* is of type ``void *`` to allow passing any
pointer object for this parameter. The provided argument should be a
pointer to a pointer of arbitrary type (e.g., ``void **``).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Free_mem`
