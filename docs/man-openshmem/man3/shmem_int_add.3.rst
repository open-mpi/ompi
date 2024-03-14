.. _shmem_int_add:


shmem_int_add
=============

.. include_body

:ref:`shmem_int_add`\ (3), shmem_int4_add\ (3), shmem_int8_add\ (3),
:ref:`shmem_long_add`\ (3), :ref:`shmem_longlong_add`\ (3) - Performs an atomic
add operation.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_int_add(int *target, int value, int pe)
   void shmem_long_add(long *target, long value, int pe)
   void shmem_longlong_add(long long *target, long long value,
     int pe)

Fortran:

.. code-block:: fortran

   include 'mpp/shmem.h'

   INTEGER pe

   CALL SHMEM_INT4_ADD(target, value, pe)
   CALL SHMEM_INT8_ADD(target, value, pe)


DESCRIPTION
-----------

The atomic add routines add **value** to the data at address **target**
on PE **pe**. The operation completes without the possibility of another
process updating target between the time of the fetch and the update.

The arguments are as follows:

target
   The remotely accessible integer data object to be updated on the
   remote PE. If you are using C/C++, the type of target should match
   that implied in the SYNOPSIS section. If you are using the Fortran
   compiler, it must be of type integer with an element size of 4 bytes
   for SHMEM_INT4_ADD and 8 bytes for SHMEM_INT8_ADD.

value
   The value to be atomically added to target. If you are using C/C++,
   the type of value should match that implied in the SYNOPSIS section.
   If you are using Fortran, it must be of type integer with an element
   size of target.

pe
   An integer that indicates the PE number upon which target is to be
   updated. If you are using Fortran, it must be a default integer
   value.


NOTES
-----

The term remotely accessible is defined in *intro_shmem*\ (3).


.. seealso::
   *intro_shmem*\ (3) *shmem_cache*\ (3)
