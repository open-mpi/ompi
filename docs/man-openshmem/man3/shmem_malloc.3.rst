.. _shmem_malloc:


shmem_malloc
============

.. include_body

:ref:`shmem_malloc`\ (3), :ref:`shmem_free`\ (3), :ref:`shmem_align`\ (3),
:ref:`shmem_realloc`\ (3) *shmalloc*\ (3), *shfree*\ (3), *shmemalign*\ (3),
*shrealloc*\ (3) - Symmetric heap memory management functions.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void *shmem_malloc(size_t size)
   void *shmalloc(size_t size)

   void shmem_free(void *ptr)
   void shfree(void *ptr)

   void *shmem_realloc(void *ptr, size_t size)
   void *shrealloc(void *ptr, size_t size)

   void *shmem_align(size_t alignment, size_t size)
   void *shmemalign(size_t alignment, size_t size)

   extern long malloc_error


DESCRIPTION
-----------

The :ref:`shmem_malloc` function returns a pointer to a block of at least
size bytes suitably aligned for any use. This space is allocated from
the symmetric heap (in contrast to *malloc*\ (3), which allocates from
the private heap).

The :ref:`shmem_align` function allocates a block in the symmetric heap
that has a byte alignment specified by the alignment argument.

The :ref:`shmem_free` function causes the block to which ptr points to, to
be deallocated, that is, made available for further allocation. If ptr
is a null pointer, no action occurs; otherwise, if the argument does not
match a pointer earlier returned by a symmetric heap function, or if the
space has already been deallocated, malloc_error is set to indicate the
error, and shfree returns.

The :ref:`shmem_realloc` function changes the size of the block to which
ptr points to, to the size (in bytes) specified by size.

The contents of the block are unchanged up to the lesser of the new and
old sizes. If the new size is larger, the value of the newly allocated
portion of the block is indeterminate. If ptr is a null pointer, the
shrealloc function behaves like the shmalloc function for the specified
size. If size is 0 and ptr is not a null pointer, the block to which it
points to is freed. Otherwise, if ptr does not match a pointer earlier
returned by a symmetric heap function, or if the space has already been
deallocated, the malloc_error variable is set to indicate the error, and
shrealloc returns a null pointer. If the space cannot be allocated, the
block to which ptr points to is unchanged.

The :ref:`shmem_malloc`, :ref:`shmem_free`, and :ref:`shmem_realloc` functions are provided
so that multiple PEs in an application can allocate symmetric, remotely
accessible memory blocks. These memory blocks can then be used with
(shmem) communication routines. Each of these functions call the
:ref:`shmem_barrier_all`\ (3) function before returning; this ensures that
all PEs participate in the memory allocation, and that the memory on
other PEs can be used as soon as the local PE returns.

The user is responsible for calling these functions with identical
argument(s) on all PEs; if differing size arguments are used, subsequent
calls may not return the same symmetric heap address on all PEs.


NOTES
-----

The total size of the symmetric heap is determined at job startup. One
can adjust the size of the heap using the SHMEM_SYMMETRIC_HEAP_SIZE
environment variable. See the *intro_shmem*\ (3) man page for further
details. The :ref:`shmem_malloc`, :ref:`shmem_free`, and :ref:`shmem_realloc` functions
differ from the private heap allocation functions in that all PEs in an
application must call them (a barrier is used to ensure this).


RETURN VALUES
-------------

The :ref:`shmem_malloc` function returns a pointer to the allocated space
(which should be identical on all PEs); otherwise, it returns a null
pointer (with malloc_error set). The :ref:`shmem_free` function returns no
value. The :ref:`shmem_realloc` function returns a pointer to the allocated
space (which may have moved); otherwise, it returns a null pointer (with
malloc_error set).


.. seealso::
   *intro_shmem*\ (3) *shmem_my_pe*\ (3) *shmem_init*\ (3)
