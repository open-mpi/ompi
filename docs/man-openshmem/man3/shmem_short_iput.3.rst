.. _shmem_short_iput:


shmem_short_iput
================

.. include_body

shmem_complex_iput\ (3), :ref:`shmem_double_iput`\ (3),
:ref:`shmem_float_iput`\ (3), :ref:`shmem_int_iput`\ (3),
shmem_integer_iput\ (3), shmem_iput4\ (3), shmem_iput8\ (3),
:ref:`shmem_iput32`\ (3), :ref:`shmem_iput64`\ (3), :ref:`shmem_iput128`\ (3),
shmem_logical_iput\ (3), :ref:`shmem_long_iput`\ (3),
:ref:`shmem_longdouble_iput`\ (3), :ref:`shmem_longlong_iput`\ (3),
shmem_real_iput\ (3), :ref:`shmem_short_iput`\ (3) - Transfer strided data
to a specified processing element (PE).


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_double_iput(double *target, const double *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_float_iput(float *target, const float *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_int_iput(int *target, const int *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_iput32(void *target, const void *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_iput64(void *target, const void *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_iput128(void *target, const void *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_long_iput(long *target, const long *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

   void shmem_longdouble_iput(long double *target,
     const long double *source, ptrdiff_t tst, ptrdiff_t sst,
     size_t len, int pe)

   void shmem_longlong_iput(long long *target,
     const long long *source, ptrdiff_t tst, ptrdiff_t sst,
     size_t len, int pe)

   void shmem_short_iput(short *target, const short *source,
     ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER tst, sst, len, pe

   CALL SHMEM_COMPLEX_IPUT(target, source, tst, sst, len,
   & pe)

   CALL SHMEM_DOUBLE_IPUT(target, source, tst, sst, len,
   & pe)

   CALL SHMEM_INTEGER_IPUT(target, source, tst, sst, len,
   & pe)

   CALL SHMEM_IPUT4(target, source, tst, sst, len, pe)

   CALL SHMEM_IPUT8(target, source, tst, sst, len, pe)

   CALL SHMEM_IPUT32(target, source, tst, sst, len, pe)

   CALL SHMEM_IPUT64(target, source, tst, sst, len, pe)

   CALL SHMEM_IPUT128(target, source, tst, sst, len, pe)

   CALL SHMEM_LOGICAL_IPUT(target, source, tst, sst, len,
   & pe)

   CALL SHMEM_REAL_IPUT(target, source, tst, sst, len, pe)


DESCRIPTION
-----------

The shmem_iput routines read the elements of a local array (**source**)
and write them to a remote array (**target**) on the PE indicated by
**pe**. These routines return when the data has been copied out of the
source array on the local PE but not necessarily before the data has
been delivered to the remote data object.

The arguments are as follows:

target
   Array to be updated on the remote PE. This data object must be
   remotely accessible.

source
   Array containing the data to be copied.

tst
   The stride between consecutive elements of the target array. The
   stride is scaled by the element size of the target array. A value of
   1 indicates contiguous data. tst must be of type integer. If you are
   using Fortran, it must be a default integer value.

sst
   The stride between consecutive elements of the source array. The
   stride is scaled by the element size of the source array. A value of
   1 indicates contiguous data. sst must be of type integer. If you are
   using Fortran, it must be a default integer value.

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

:ref:`shmem_iput32`, shmem_iput4: Any noncharacter type that has a storage size equal
   to 32 bits.

:ref:`shmem_iput64`, shmem_iput8: Any noncharacter type that has a storage size equal
   to 64 bits.

:ref:`shmem_iput128`: Any noncharacter type that has a storage size equal to 128 bits.

:ref:`shmem_short_iput`: Elements of type short.

:ref:`shmem_int_iput`: Elements of type int.

:ref:`shmem_long_iput`: Elements of type long.

:ref:`shmem_longlong_iput`: Elements of type long long.

:ref:`shmem_float_iput`: Elements of type float.

:ref:`shmem_double_iput`: Elements of type double.

:ref:`shmem_longdouble_iput`: Elements of type long double.

**SHMEM_COMPLEX_IPUT**: Elements of type complex of default size.

**SHMEM_DOUBLE_IPUT**: (Fortran) Elements of type double precision.

**SHMEM_INTEGER_IPUT**: Elements of type integer.

**SHMEM_LOGICAL_IPUT**: Elements of type logical.

**SHMEM_REAL_IPUT**: Elements of type real.

**SHMEM_LOGICAL_IPUT**: Elements of type logical.

**SHMEM_REAL_IPUT**: Elements of type real.

If you are using Fortran, data types must be of default size. For
example, a real variable must be declared as REAL, REAL*4 or
REAL(KIND=4).


NOTES
-----

See *intro_shmem*\ (3) for a definition of the term remotely accessible.


EXAMPLES
--------

Consider the following simple :ref:`shmem_long_iput` example for C/C++
programs.

::

   #include <mpp/shmem.h>

   main()
   {
     short source[10] = { 1, 2, 3, 4, 5,
     6, 7, 8, 9, 10 };
     static short target[10];

     shmem_init();
     if (shmem_my_pe() == 0) {
       /* put 10 words into target on PE 1 */
       shmem_short_iput(target, source, 1, 2, 5, 1);
     }
     shmem_barrier_all(); /* sync sender and receiver */
     if (shmem_my_pe() == 1) {
       shmem_udcflush(); /* not required on IRIX systems */
       printf("target on PE %d is %d %d %d %d %d0, shmem_my_pe(),
       (int)target[0], (int)target[1], (int)target[2],
       (int)target[3], (int)target[4] );
     }
     shmem_barrier_all(); /* sync before exiting */
   }


.. seealso::
   *intro_shmem*\ (3) *shmem_iget*\ (3) *shmem_put*\ (3) *shmem_quiet*\ (3)
