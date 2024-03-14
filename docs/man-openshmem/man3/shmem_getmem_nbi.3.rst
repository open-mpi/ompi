.. _shmem_getmem_nbi:


shmem_getmem_nbi
================

.. include_body

:ref:`shmem_getmem_nbi`\ (3), :ref:`shmem_char_get_nbi`\ (3),
:ref:`shmem_short_get_nbi`\ (3), :ref:`shmem_int_get_nbi`\ (3),
:ref:`shmem_long_get_nbi`\ (3), :ref:`shmem_longlong_get_nbi`\ (3),
:ref:`shmem_float_get_nbi`\ (3), :ref:`shmem_double_get_nbi`\ (3),
:ref:`shmem_longdouble_get_nbi`\ (3), :ref:`shmem_get8_nbi`\ (3),
:ref:`shmem_get16_nbi`\ (3), :ref:`shmem_get32_nbi`\ (3), :ref:`shmem_get64_nbi`\ (3),
:ref:`shmem_get128_nbi`\ (3), - The nonblocking get routines provide a method
for copying data from a contiguous remote data object on the specified
PE to the local data object.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_getmem_nbi(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_char_get(char *dest, const char *source,
     size_t nelems, int pe)

   void shmem_short_get(short *dest, const short *source,
     size_t nelems, int pe)

   void shmem_int_get(int *dest, const int *source,
     size_t nelems, int pe)

   void shmem_long_get(long *dest, const long *source,
     size_t nelems, int pe)

   void shmem_longlong_get(long long *dest, const long long *source,
     size_t nelems, int pe)

   void shmem_float_get(float *dest, const float *source,
     size_t nelems, int pe)

   void shmem_double_get(double *dest, const double *source,
     size_t nelems, int pe)

   void shmem_longdouble_get(long double *dest, const long double *source,
     size_t nelems, int pe)

   void shmem_get8(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_get16(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_get32(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_get64(void *dest, const void *source,
     size_t nelems, int pe)

   void shmem_get128(void *dest, const void *source,
     size_t nelems, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER nelems, pe

   CALL SHMEM_GETMEM_NBI(dest, source, nelems, pe)

   CALL SHMEM_CHARACTER_GET_NBI(dest, source, nelems, pe)

   CALL SHMEM_COMPLEX_GET_NBI(dest, source, nelems, pe)

   CALL SHMEM_DOUBLE_GET_NBI(dest, source, nelems, pe)

   CALL SHMEM_INTEGER_GET_NBI(dest, source, nelems, pe)

   CALL SHMEM_LOGICAL_GET_NBI(dest, source, nelems, pe)

   CALL SHMEM_REAL_GET_NBI(dest, source, nelems, pe)

   CALL SHMEM_GET4_NBI(dest, source, nelems, pe)

   CALL SHMEM_GET8_NBI(dest, source, nelems, pe)

   CALL SHMEM_GET32_NBI(dest, source, nelems, pe)

   CALL SHMEM_GET64_NBI(dest, source, nelems, pe)

   CALL SHMEM_GET128_NBI(dest, source, nelems, pe)


DESCRIPTION
-----------

The get routines provide a method for copying a contiguous symmetric
data object from a different PE to a contiguous data object on the local
PE. The routines return after posting the operation. The operation is
considered complete after a subsequent call to :ref:`shmem_quiet`. At the
completion of :ref:`shmem_quiet`, the data has been delivered to the dest array
on the local PE.

The arguments are as follows:

dest
   Local data object to be updated.

source
   Data object on the PE identified by pe that contains the data to be
   copied. This data object must be remotely accessible.

nelems
   Number of elements in the target and source arrays. len must be of
   type integer. If you are using Fortran, it must be a constant,
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

Consider this simple example for Fortran.

.. code-block:: fortran

   PROGRAM REDUCTION
     REAL VALUES, SUM
     COMMON /C/ VALUES
     REAL WORK

     CALL START_PES(0) ! ALLOW ANY NUMBER OF PES
     VALUES = MY_PE() ! INITIALIZE IT TO SOMETHING
     CALL SHMEM_BARRIER_ALL
     SUM = 0.0
     DO I = 0,NUM_PES()-1
       CALL SHMEM_REAL_GET_NBI(WORK, VALUES, 1, I)
       CALL SHMEM_QUIET                ! wait for delivery
       SUM = SUM + WORK
     ENDDO
     PRINT *, 'PE ', MY_PE(), ' COMPUTED SUM=', SUM
     CALL SHMEM_BARRIER_ALL
   END


.. seealso::
   *intro_shmem*\ (3) *shmem_quiet*\ (3)
