.. _shmem_int_set:


shmem_int_set
=============

.. include_body

:ref:`shmem_double_set`\ (3), :ref:`shmem_float_set`\ (3), :ref:`shmem_int_set`\ (3),
:ref:`shmem_long_set`\ (3), :ref:`shmem_longlong_set`\ (3) shmem_int4_set\ (3),
shmem_int8_set\ (3), shmem_real4_set\ (3), shmem_real8_set\ (3), -
Atomically sets the value of a remote data object


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_double_set(double *target, double value, int pe)

   void shmem_float_set(float *target, float value, int pe)

   void shmem_int_set(int *target, int value, int pe)

   void shmem_long_set(long *target, long value, int pe)

   void shmem_longlong_set(long long *target, long long value, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER pe

   CALL SHMEM_INT4_SET(target, value, pe)
   CALL SHMEM_INT8_SET(target, value, pe)
   CALL SHMEM_REAL4_SET(target, value, pe)
   CALL SHMEM_REAL8_SET(target, value, pe)


DESCRIPTION
-----------

The set routines write the **value** into the address **target** on
**pe** as an atomic operation.

The arguments are as follows:

target
   The remotely accessible data object to be set on the remote PE.

value
   The value to be atomically written to the remote PE.

pe
   An integer that indicates the PE number upon which target is to be
   updated. If you are using Fortran, it must be a default integer
   value.


RETURN VALUES
-------------

NONE


.. seealso::
   *intro_shmem*\ (3)
