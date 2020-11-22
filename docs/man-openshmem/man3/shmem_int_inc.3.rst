.. _shmem_int_inc:


shmem_int_inc
=============

.. include_body

shmem_int4_inc\ (3), shmem_int8_inc\ (3), :ref:`shmem_int_inc`\ (3),
:ref:`shmem_long_inc`\ (3), :ref:`shmem_longlong_inc`\ (3) - These routines
perform an atomic increment operation on a remote data object.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   int shmem_int_inc(int *target, int pe)

   long shmem_long_inc(long *target, int pe)

   long long shmem_longlong_inc(long long *target, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER pe
   INTEGER(KIND=4) SHMEM_INT4_INC, target4
   INTEGER(KIND=8) SHMEM_INT8_INC, target8

   ires4 = SHMEM_INT4_INC(target4, pe)

   ires8 = SHMEM_INT8_INC(target8, pe)


DESCRIPTION
-----------

The atomic increment routines replace the value of **target** with its
value incremented by one. The operation must be completed without the
possibility of another process updating **target** between the time of
the fetch and the update.

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

None.


.. seealso::
   *intro_shmem*\ (3)
