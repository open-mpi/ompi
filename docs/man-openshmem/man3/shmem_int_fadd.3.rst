.. _shmem_int_fadd:


shmem_int_fadd
==============

.. include_body

shmem_int4_fadd\ (3), shmem_int8_fadd\ (3), :ref:`shmem_int_fadd`\ (3),
:ref:`shmem_long_fadd`\ (3), :ref:`shmem_longlong_fadd`\ (3) - Performs an atomic
fetch-and-add operation on a remote data object


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   int shmem_int_fadd(int *target, int value, int pe)

   long shmem_long_fadd(long *target, long value, int pe)

   long long shmem_longlong_fadd(long long *target, longlong value,
     int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER pe

   INTEGER(KIND=4) SHMEM_INT4_FADD, ires, target, value
   ires = SHMEM_INT4_FADD(target, value, pe)

   INTEGER(KIND=8) SHMEM_INT8_FADD, ires, target, value
   ires = SHMEM_INT8_FADD(target, value, pe)


DESCRIPTION
-----------

shmem_fadd functions perform an atomic fetch-and-add operation. An
atomic fetch-and-add operation fetches the old target and adds value to
target without the possibility of another process updating target
between the time of the fetch and the update. These routines add value
to target on Processing Element (PE) pe and return the previous contents
of target as an atomic operation.

The arguments are as follows:

target
   The remotely accessible integer data object to be updated on the
   remote PE. The type of target should match that implied in the
   SYNOPSIS section.

value
   The value to be atomically added to target. The type of value should
   match that implied in the SYNOPSIS section.

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
to the atomic addition operation.


.. seealso::
   *intro_shmem*\ (3)
