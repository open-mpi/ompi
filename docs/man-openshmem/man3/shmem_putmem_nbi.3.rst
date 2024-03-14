.. _shmem_putmem_nbi:


shmem_putmem_nbi
================

.. include_body

:ref:`shmem_putmem_nbi`\ (3), :ref:`shmem_char_put_nbi`\ (3),
:ref:`shmem_short_put_nbi`\ (3), :ref:`shmem_int_put_nbi`\ (3),
:ref:`shmem_long_put_nbi`\ (3), :ref:`shmem_longlong_put_nbi`\ (3),
:ref:`shmem_float_put_nbi`\ (3), :ref:`shmem_double_put_nbi`\ (3),
:ref:`shmem_longdouble_put_nbi`\ (3), :ref:`shmem_put8_nbi`\ (3),
:ref:`shmem_put16_nbi`\ (3), :ref:`shmem_put32_nbi`\ (3), :ref:`shmem_put64_nbi`\ (3),
:ref:`shmem_put128_nbi`\ (3), - The nonblocking put routines provide a method
for copying data from a contiguous local data object to a data object on
a specified PE.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_putmem_nbi(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_char_put(char *dest, const char *source,
     size_t nelems, int pe)

   void shmem_short_put(short *dest, const short *source,
     size_t nelems, int pe)

   void shmem_int_put(int *dest, const int *source,
     size_t nelems, int pe)

   void shmem_long_put(long *dest, const long *source,
     size_t nelems, int pe)

   void shmem_longlong_put(long long *dest, const long long *source,
     size_t nelems, int pe)

   void shmem_float_put(float *dest, const float *source,
     size_t nelems, int pe)

   void shmem_double_put(double *dest, const double *source,
     size_t nelems, int pe)

   void shmem_longdouble_put(long double *dest, const long double *source,
     size_t nelems, int pe)

   void shmem_put8(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_put16(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_put32(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_put64(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_put128(void *dest, const void *source,
     size_t nelems, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER nelems, pe

   CALL SHMEM_PUTMEM_NBI(dest, source, nelems, pe)

   CALL SHMEM_CHARACTER_PUT_NBI(dest, source, nelems, pe)

   CALL SHMEM_COMPLEX_PUT_NBI(dest, source, nelems, pe)

   CALL SHMEM_DOUBLE_PUT_NBI(dest, source, nelems, pe)

   CALL SHMEM_INTEGER_PUT_NBI(dest, source, nelems, pe)

   CALL SHMEM_LOGICAL_PUT_NBI(dest, source, nelems, pe)

   CALL SHMEM_REAL_PUT_NBI(dest, source, nelems, pe)

   CALL SHMEM_PUT4_NBI(dest, source, nelems, pe)

   CALL SHMEM_PUT8_NBI(dest, source, nelems, pe)

   CALL SHMEM_PUT32_NBI(dest, source, nelems, pe)

   CALL SHMEM_PUT64_NBI(dest, source, nelems, pe)

   CALL SHMEM_PUT128_NBI(dest, source, nelems, pe)


DESCRIPTION
-----------

The routines return after posting the operation. The operation is
considered complete after a subsequent call to :ref:`shmem_quiet`. At the
completion of :ref:`shmem_quiet`, the data has been copied into the dest array
on the destination PE. The delivery of data words into the data object
on the destination PE may occur in any order. Furthermore, two
successive put routines may deliver data out of order unless a call to
:ref:`shmem_fence` is introduced between the two calls.

The arguments are as follows:

dest
   Data object to be updated on the remote PE. This data object must be
   remotely accessible.

source
   Data object containing the data to be copied.

nelems
   Number of elements in the dest and source arrays. nelems must be of
   type size_t for C. If you are using Fortran, it must be a constant,
   variable, or array element of default integer type.

pe
   PE number of the remote PE. pe must be of type integer. If you are
   using Fortran, it must be a constant, variable, or array element of
   default integer type.

If you are using Fortran, data types must be of default size. For
example, a real variable must be declared as REAL, REAL*4, or
REAL(KIND=4).


NOTES
-----

See *intro_shmem*\ (3) for a definition of the term remotely accessible.


EXAMPLES
--------

Consider this simple example for C.

.. code-block:: c

   #include <stdio.h>
   #include <mpp/shmem.h>

   main()
   {
     long source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
     static long target[10];
     shmem_init();

     if (shmem_my_pe() == 0) {
       /* put 10 words into target on PE 1 */
       shmem_long_put_nbi(target, source, 10, 1);
       shmem_quiet();
     }
     shmem_barrier_all();  /* sync sender and receiver */
     if (shmem_my_pe() == 1)
       shmem_udcflush();  /* not required on Altix systems */
     printf("target[0] on PE %d is %d\n", shmem_my_pe(), target[0]);
   }


.. seealso::
   *intro_shmem*\ (3) *shmem_quiet*\ (3)
