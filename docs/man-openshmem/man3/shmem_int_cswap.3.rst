.. _shmem_int_cswap:


shmem_int_cswap
===============

.. include_body

:ref:`shmem_int_cswap`\ (3), shmem_int4_cswap\ (3),
shmem_int8_cswap\ (3), :ref:`shmem_long_cswap`\ (3),
:ref:`shmem_longlong_cswap`\ (3) - Performs an atomic conditional swap to a
remote data object


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   int shmem_int_cswap(int *target, int cond, int value, int pe)

   long shmem_long_cswap(long *target, long cond, long value,
     int pe)

   long long shmem_longlong_cswap(longlong *target,
     longlong cond, longlong value, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER pe

   INTEGER(KIND=4) SHMEM_INT4_CSWAP
   ires = SHMEM_INT4_CSWAP(target, cond, value, pe)

   INTEGER(KIND=8) SHMEM_INT8_CSWAP
   ires = SHMEM_INT8_CSWAP(target, cond, value, pe)


DESCRIPTION
-----------

The conditional swap routines conditionally update a target data object
on an arbitrary processing element (PE) and return the prior contents of
the data object in one atomic operation.

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

target
   The remotely accessible integer data object to be updated on the
   remote PE. If you are using C/C++, the data type of target should
   match that implied in the SYNOPSIS section. If you are using Fortran,
   it must be of the following type:

   **SHMEM_INT4_CSWAP**: 4-byte integer

   **SHMEM_INT8_CSWAP**: 8-byte integer

cond
   cond is compared to the remote target value. If cond and the remote
   target are equal, then value is swapped into the remote target.
   Otherwise, the remote target is unchanged. In either case, the old
   value of the remote target is returned as the function return value.
   cond must be of the same data type as target.

value
   The value to be atomically written to the remote PE. value must be
   the same data type as target.

pe
   An integer that indicates the PE number upon which target is to be
   updated. If you are using Fortran, it must be a default integer
   value.


NOTES
-----

The term remotely accessible is defined in *intro_shmem*\ (3).


RETURN VALUES
-------------

The contents that had been in the target data object on the remote PE
prior to the conditional swap.


EXAMPLES
--------

The following call ensures that the first PE to execute the conditional
swap will successfully write its PE number to race_winner on PE 0.

::

   main()
   {
     static int race_winner = -1;
     int oldval;

     shmem_init();
     oldval = shmem_int_cswap(&race_winner, -1, shmem_my_pe(), 0);
     if (oldval == -1)
       printf("pe %d was first\n",shmem_my_pe());
   }


.. seealso::
   *intro_shmem*\ (3) *shmem_cache*\ (3) *shmem_swap*\ (3)
