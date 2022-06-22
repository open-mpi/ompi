.. _shmem_udcflush:


shmem_udcflush
==============

.. include_body

:ref:`shmem_clear_cache_inv`\ (3), :ref:`shmem_set_cache_inv`\ (3),
:ref:`shmem_set_cache_line_inv`\ (3), :ref:`shmem_udcflush`\ (3),
:ref:`shmem_udcflush_line`\ (3) - Controls data cache utilities


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_clear_cache_inv(void)
   void shmem_clear_cache_line_inv(void *target)
   void shmem_set_cache_inv(void)
   void shmem_set_cache_line_inv(void *target)
   void shmem_udcflush(void)
   void shmem_udcflush_line(void *target)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   CALL SHMEM_CLEAR_CACHE_INV
   CALL SHMEM_CLEAR_CACHE_LINE_INV(target)
   CALL SHMEM_SET_CACHE_INV
   CALL SHMEM_SET_CACHE_LINE_INV(target)

   CALL SHMEM_UDCFLUSH
   CALL SHMEM_UDCFLUSH_LINE(target)


DESCRIPTION
-----------

The following argument is passed to the cache line control routines:

target
   A data object that is local to the processing element (PE). target
   can be of any noncharacter type. If you are using Fortran, it can be
   of any kind.

:ref:`shmem_clear_cache_inv` disables automatic cache coherency mode
previously enabled by :ref:`shmem_set_cache_inv` or :ref:`shmem_set_cache_line_inv`.

:ref:`shmem_clear_cache_line_inv` disables automatic cache coherency mode
for the cache line associated with the address of **target** only.

:ref:`shmem_set_cache_inv` enables the OpenSHMEM API to automatically
decide the best strategy for cache coherency.

:ref:`shmem_set_cache_line_inv` enables automatic cache coherency mode for
the cache line associated with the address of **target** only.

:ref:`shmem_clear_cache_inv` disables automatic cache coherency mode
previously enabled by :ref:`shmem_set_cache_inv` or :ref:`shmem_set_cache_line_inv`.

:ref:`shmem_udcflush` makes the entire user data cache coherent.

:ref:`shmem_udcflush_line` makes coherent the cache line that corresponds
with the address specified by target.


NOTES
-----

These routines have been retained for improved backward compatibility
with legacy architectures.


.. seealso::
   *intro_shmem*\ (3) *shmem_put*\ (3) *shmem_swap*\ (3)
