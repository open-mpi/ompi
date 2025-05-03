.. _mpi_alloc_mem:


MPI_Alloc_mem
=============

.. include_body

:ref:`MPI_Alloc_mem` |mdash| Allocates a specified memory segment.

.. The following file was automatically generated
.. include:: ./bindings/mpi_alloc_mem.rst

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


Fortran NOTES
-------------

The :ref:`MPI_Alloc_mem` calls require the use of the ``iso_c_binding`` module
for due to the use of ``TYPE(C_PTR)``.

.. code-block:: fortran

   use iso_c_binding

   type(c_ptr) :: alloc_ptr
   integer :: size, ierr
   double precision, pointer :: array(:,:)

   ! A 2D array of 100 elements
   size = 10 * 10
   call MPI_Alloc_Mem(size * 8, MPI_INFO_NULL, alloc_ptr, ierr)

   ! Point to the array
   call c_f_pointer(alloc_ptr, array, [10, 10])

   ! ... use the array ...

   ! Free the memory, no need for the alloc_ptr
   call MPI_Free_mem(array, ierr)


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Free_mem`
