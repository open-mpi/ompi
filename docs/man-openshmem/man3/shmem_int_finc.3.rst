.. _shmem_int_finc:


shmem_int_finc
==============

.. include_body

shmem_int4_finc\ (3), shmem_int8_finc\ (3), :ref:`shmem_int_finc`\ (3),
:ref:`shmem_long_finc`\ (3), :ref:`shmem_longlong_finc`\ (3) - Performs an atomic
fetch-and-increment operation on a remote data object


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   int shmem_int_finc(int *target, int pe)

   long shmem_long_finc(long *target, int pe)

   long long shmem_longlong_finc(long long *target, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER pe
   INTEGER(KIND=4) SHMEM_INT4_FINC, target4
   INTEGER(KIND=8) SHMEM_INT8_FINC, target8

   ires4 = SHMEM_INT4_FINC(target4, pe)

   ires8 = SHMEM_INT8_FINC(target8, pe)


DESCRIPTION
-----------

The fetch and increment routines retrieve the value at address
**target** on PE **pe**, and update **target** with the result of
incrementing the retrieved value by one. The operation must be completed
without the possibility of another process updating **target** between
the time of the fetch and the update.

The arguments are as follows:

target
   The remotely accessible integer data object to be updated on the
   remote PE. The type of target should match that implied in the
   SYNOPSIS section.

pe
   An integer that indicates the PE number upon which target is to be
   updated. If you are using Fortran, it must be a default integer
   value.


NOTES
-----

The term remotely accessible is defined in *intro_shmem*\ (3).


RETURN VALUES
-------------

The contents that had been at the target address on the remote PE prior
to the increment.


.. seealso::
   *intro_shmem*\ (3)
