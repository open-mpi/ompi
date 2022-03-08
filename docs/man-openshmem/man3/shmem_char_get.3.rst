.. _shmem_char_get:


shmem_char_get
==============

.. include_body

shmem_character_get\ (3), shmem_complex_get\ (3),
:ref:`shmem_double_get`\ (3), :ref:`shmem_float_get`\ (3), shmem_get4\
(3), shmem_get8\ (3), :ref:`shmem_get32`\ (3), :ref:`shmem_get64`\
(3), :ref:`shmem_get128`\ (3), :ref:`shmem_getmem`\ (3),
:ref:`shmem_int_get`\ (3), shmem_integer_get\ (3), shmem_logical_get\
(3), :ref:`shmem_long_get`\ (3), :ref:`shmem_longdouble_get`\ (3),
:ref:`shmem_longlong_get`\ (3), shmem_real_get\ (3),
:ref:`shmem_short_get`\ (3) - Transfers data from a specified
processing element (PE).


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_get32(void *target, const void *source,
     size_t len, int pe)

   void shmem_get64(void *target, const void *source,
     size_t len, int pe)

   void shmem_get128(void *target, const void *source,
     size_t len, int pe)

   void shmem_getmem(void *target, const void *source,
     size_t len, int pe)

   void shmem_int_get(int *target, const int *source,
     size_t len, int pe)

   void shmem_double_get(double *target, const double *source,
     size_t len, int pe)

   void shmem_float_get(float *target, const float *source,
     size_t len, int pe)

   void shmem_long_get(long *target, const long *source,
     size_t len, int pe)

   void shmem_longdouble_get(long double *target,
     const long double *source, size_t len, int pe)

   void shmem_longlong_get(long long *target,
     const long long *source, size_t len, int pe)

   void shmem_short_get(short *target,
     const short *source, size_t len, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER len, pe

   CALL SHMEM_CHARACTER_GET(target, source, len, pe)

   CALL SHMEM_COMPLEX_GET(target, source, len, pe)

   CALL SHMEM_DOUBLE_GET(target, source, len, pe)

   CALL SHMEM_GET4(target, source, len, pe)

   CALL SHMEM_GET8(target, source, len, pe)

   CALL SHMEM_GET32(target, source, len, pe)

   CALL SHMEM_GET64(target, source, len, pe)

   CALL SHMEM_GET128(target, source, len, pe)

   CALL SHMEM_GETMEM(target, source, len, pe)

   CALL SHMEM_INTEGER_GET(target, source, len, pe)

   CALL SHMEM_LOGICAL_GET(target, source, len, pe)

   CALL SHMEM_REAL_GET(target, source, len, pe)


DESCRIPTION
-----------

The shmem_get routines transfer **nelems** elements of the data object
at address **source** on the remote PE **pe**, to the data object at
address **target** on the local PE. These routines return after the data
has been copied to address **target** on the local PE.

The arguments are as follows:

target
   Local data object to be updated.

source
   Data object on the PE identified by pe that contains the data to be
   copied. This data object must be remotely accessible.

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

:ref:`shmem_getmem`: Fortran: Any noncharacter type. C: Any data type. len is
   scaled in bytes.

shmem_get4, :ref:`shmem_get32`: Any noncharacter type that has a storage size
   equal to 32 bits.

{shmem_get8, :ref:`shmem_get64`}: Any noncharacter type that has a storage size equal to
   64 bits.

:ref:`shmem_get128`: Any noncharacter type that has a storage size equal to 128
   bits.

:ref:`shmem_short_get`: Elements of type short.

:ref:`shmem_int_get`: Elements of type int.

:ref:`shmem_long_get`: Elements of type long.

:ref:`shmem_longlong_get`: Elements of type long long.

:ref:`shmem_float_get`: Elements of type float.

:ref:`shmem_double_get`: Elements of type double.

:ref:`shmem_longdouble_get`: Elements of type long double.

**SHMEM_CHARACTER_GET**: Elements of type character. len is the number of
   characters to transfer. The actual character lengths of the source
   and target variables are ignored.

**SHMEM_COMPLEX_GET**: Elements of type complex of default size.

**SHMEM_DOUBLE_GET**: (Fortran) Elements of type double precision.

**SHMEM_INTEGER_GET**: Elements of type integer.

**SHMEM_LOGICAL_GET**: Elements of type logical.

**SHMEM_REAL_GET**: Elements of type real.

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
       CALL SHMEM_REAL_GET(WORK, VALUES, 1, I)
       SUM = SUM + WORK
     ENDDO
     PRINT *, 'PE ', MY_PE(), ' COMPUTED SUM=', SUM
     CALL SHMEM_BARRIER_ALL
   END


.. seealso::
   *intro_shmem*\ (3) *shmem_put*\ (3) *shmem_iget*\ (3) *shmem_quiet*\ (3)
