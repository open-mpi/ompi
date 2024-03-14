.. _shmem_short_iget:


shmem_short_iget
================

.. include_body

shmem_complex_iget\ (3), :ref:`shmem_double_iget`\ (3),
:ref:`shmem_float_iget`\ (3), shmem_iget4\ (3), shmem_iget8\ (3),
:ref:`shmem_iget32`\ (3), :ref:`shmem_iget64`\ (3), :ref:`shmem_iget128`\ (3),
:ref:`shmem_int_iget`\ (3), shmem_integer_iget\ (3),
shmem_logical_iget\ (3), :ref:`shmem_long_iget`\ (3),
:ref:`shmem_longdouble_iget`\ (3), :ref:`shmem_longlong_iget`\ (3),
shmem_real_iget\ (3), :ref:`shmem_short_iget`\ (3) - Transfers strided data
from a specified processing element (PE)


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_iget32(void *target, const void *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_iget64(void *target, const void *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_iget128(void *target, const void *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_int_iget(int *target, const int *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_double_iget(double *target, const double *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_float_iget(float *target, const float *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_long_iget(long *target, const long *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_longdouble_iget(long double *target,
     const long double *source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe)

   void shmem_longlong_iget(long long *target,
     const long long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_short_iget(short *target,
     const short *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER tst, sst, len, pe

   CALL SHMEM_COMPLEX_IGET(target, source, tst, sst, len,
   & pe)

   CALL SHMEM_DOUBLE_IGET(target, source, tst, sst, len,
   & pe)

   CALL SHMEM_IGET4(target, source, tst, sst, len, pe)

   CALL SHMEM_IGET8(target, source, tst, sst, len, pe)

   CALL SHMEM_IGET32(target, source, tst, sst, len, pe)

   CALL SHMEM_IGET64(target, source, tst, sst, len, pe)

   CALL SHMEM_IGET128(target, source, tst, sst, len, pe)

   CALL SHMEM_INTEGER_IGET(target, source, tst, sst, len,
   & pe)

   CALL SHMEM_LOGICAL_IGET(target, source, tst, sst, len,
   & pe)

   CALL SHMEM_REAL_IGET(target, source, tst, sst, len, pe)


DESCRIPTION
-----------

The strided get routines retrieve array data available at address source
on remote PE (pe). The elements of the **source** array are separated by
a stride **sst**. Once the data is received, it is stored at the local
memory address **target**, separated by stride **tst**. The routines
return when the data has been copied into the local **target** array.

The arguments are as follows:

target
   Array to be updated on the local PE.

source
   Array containing the data to be copied on the remote PE.

tst
   The stride between consecutive elements of the target array. The
   stride is scaled by the element size of the target array. A value of
   1 indicates contiguous data. tst must be of type integer. If you are
   calling from Fortran, it must be a default integer value.

sst
   The stride between consecutive elements of the source array. The
   stride is scaled by the element size of the source array. A value of
   1 indicates contiguous data. sst must be of type integer. If you are
   calling from Fortran, it must be a default integer value.

len
   Number of elements in the target and source arrays. len must be of
   type integer. If you are using Fortran, it must be a constant,
   variable, or array element of default integer type.

pe
   PE number of the remote PE. pe must be of type integer. If you are
   using Fortran, it must be a constant, variable, or array element of
   default integer type.

The target and source data objects must conform to typing constraints,
which are as follows:

:ref:`shmem_iget32`, shmem_iget4: Any noncharacter type that has a storage size
   equal to 32 bits.

:ref:`shmem_iget64`, shmem_iget8: Any noncharacter type that has a storage size
   equal to 64 bits.

:ref:`shmem_iget128`: Any noncharacter type that has a storage size equal to
   128 bits.

:ref:`shmem_short_iget`: Elements of type short.

:ref:`shmem_int_iget`: Elements of type int.

:ref:`shmem_long_iget`: Elements of type long.

:ref:`shmem_longlong_iget`: Elements of type long long.

:ref:`shmem_float_iget`: Elements of type float.

:ref:`shmem_double_iget`: Elements of type double.

:ref:`shmem_longdouble_iget`: Elements of type long double.

**SHMEM_COMPLEX_IGET**: Elements of type complex of default size.

**SHMEM_DOUBLE_IGET**: (Fortran) Elements of type double precision.

**SHMEM_INTEGER_IGET**: Elements of type integer.

**SHMEM_LOGICAL_IGET**: Elements of type logical.

**SHMEM_REAL_IGET**: Elements of type real.

:ref:`shmem_longdouble_iget`: Elements of type long double.

**SHMEM_COMPLEX_IGET**: Elements of type complex of default size.

**SHMEM_DOUBLE_IGET**: (Fortran) Elements of type double precision.

**SHMEM_INTEGER_IGET**: Elements of type integer.

**SHMEM_LOGICAL_IGET**: Elements of type logical.

**SHMEM_REAL_IGET**: Elements of type real.

If you are using Fortran, data types must be of default size. For
example, a real variable must be declared as REAL, REAL*4, or
REAL(KIND=4).


NOTES
-----

See *intro_shmem*\ (3) for a definition of the term remotely accessible.


EXAMPLES
--------

The following simple example uses shmem_logical_iget in a Fortran
program. Compile this example with the -lsma compiler option.

::

   PROGRAM STRIDELOGICAL
     LOGICAL SOURCE(10), TARGET(5)
     SAVE SOURCE ! SAVE MAKES IT REMOTELY ACCESSIBLE
     DATA SOURCE /.T.,.F.,.T.,.F.,.T.,.F.,.T.,.F.,.T.,.F./
     DATA TARGET / 5*.F. /

     CALL START_PES(2)
     IF (MY_PE() .EQ. 0) THEN
       CALL SHMEM_LOGICAL_IGET(TARGET, SOURCE, 1, 2, 5, 1)
       PRINT*,'TARGET AFTER SHMEM_LOGICAL_IGET:',TARGET
     ENDIF
     CALL SHMEM_BARRIER_ALL
   END


.. seealso::
   *intro_shmem*\ (3) *shmem_get*\ (3) *shmem_quiet*\ (3)
