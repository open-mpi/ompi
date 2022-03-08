.. _shmem_int_fetch:


shmem_int_fetch
===============

.. include_body

shmem_int4_fetch\ (3), shmem_int8_fetch\ (3),
:ref:`shmem_int_fetch`\ (3), :ref:`shmem_long_fetch`\ (3),
:ref:`shmem_longlong_fetch`\ (3) :ref:`shmem_double_fetch`\ (3)
:ref:`shmem_float_fetch`\ (3) - Atomically fetches the value of a remote data
object


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   int shmem_int_fetch(int *target, int pe)

   long shmem_long_fetch(long *target, int pe)

   long long shmem_longlong_fetch(long long *target, int pe)

   double shmem_double_fetch(long long *target, int pe)

   float  shmem_float_fetch(float *target, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER pe

   INTEGER(KIND=4) SHMEM_INT4_FETCH, ires, target
   ires = SHMEM_INT4_FETCH(target, pe)

   INTEGER(KIND=8) SHMEM_INT8_FETCH, ires, target
   ires = SHMEM_INT8_FETCH(target, pe)


   REAL(KIND=4) SHMEM_INT4_FETCH, ires, target
   ires = SHMEM_REAL4_FETCH(target, pe)

   REAL(KIND=8) SHMEM_INT8_FETCH, ires, target
   ires = SHMEM_REAL8_FETCH(target, pe)


DESCRIPTION
-----------

The shmem_fetch functions perform an atomic fetch operation. They return
the contents of the **target** as an atomic operation.

The arguments are as follows:

target
   The remotely accessible data object to be fetched from the remote PE.

pe
   An integer that indicates the PE number from which *target* is to be
   fetched. If you are using Fortran, it must be a default integer
   value.


RETURN VALUES
-------------

The contents at the *target* address on the remote PE. The data type of
the return value is the same as the the type of the remote data object.


.. seealso::
   *intro_shmem*\ (3)
