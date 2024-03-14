.. _shmem_swap:


shmem_swap
==========

.. include_body

:ref:`shmem_double_swap`\ (3), :ref:`shmem_float_swap`\ (3),
:ref:`shmem_int_swap`\ (3), :ref:`shmem_long_swap`\ (3), :ref:`shmem_swap`\ (3),
shmem_int4_swap\ (3), shmem_int8_swap\ (3), shmem_real4_swap\ (3),
shmem_real8_swap\ (3), :ref:`shmem_longlong_swap`\ (3) - Performs an atomic
swap to a remote data object


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   double shmem_double_swap(double *target, double value,
     int pe)

   float shmem_float_swap(float *target, float value, int pe)

   int shmem_int_swap(int *target, int value, int pe)

   long shmem_long_swap(long *target, long value, int pe)

   long long shmem_longlong_swap(long long *target,
     long long value, int pe)

   long shmem_swap(long *target, long value, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER pe

   INTEGER SHMEM_SWAP
   ires = SHMEM_SWAP(target, value, pe)

   INTEGER(KIND=4) SHMEM_INT4_SWAP
   ires = SHMEM_INT4_SWAP(target, value, pe)

   INTEGER(KIND=8) SHMEM_INT8_SWAP
   ires = SHMEM_INT8_SWAP(target, value, pe)

   REAL(KIND=4) SHMEM_REAL4_SWAP
   res = SHMEM_REAL4_SWAP(target, value, pe)

   REAL(KIND=8) SHMEM_REAL8_SWAP
   res = SHMEM_REAL8_SWAP(target, value, pe)


DESCRIPTION
-----------

The atomic swap routines write **value** to address target on PE **pe**,
and return the previous contents of **target** in one atomic operation.

The arguments are as follows:

target
   The remotely accessible integer data object to be updated on the
   remote PE. If you are using C/C++, the type of target should match
   that implied in the SYNOPSIS section. If you are using Fortran, it
   must be of the following type:

   **SHMEM_SWAP:** Integer of default kind

   **SHMEM_INT4_SWAP:** 4-byte integer

   **SHMEM_INT8_SWAP:** 8-byte integer

   **SHMEM_REAL4_SWAP:** 4-byte real

   **SHMEM_REAL8_SWAP:** 8-byte real

value
   Value to be atomically written to the remote PE. value is the same
   type as target.

pe
   An integer that indicates the PE number on which target is to be
   updated. If you are using Fortran, it must be a default integer
   value.


NOTES
-----

The term remotely accessible is defined in *intro_shmem*\ (3).


RETURN VALUES
-------------

The contents that had been at the target address on the remote PE prior
to the swap is returned.


.. seealso::
   *intro_shmem*\ (3)
