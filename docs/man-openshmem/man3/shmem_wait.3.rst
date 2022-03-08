.. _shmem_wait:


shmem_wait
==========

.. include_body

:ref:`shmem_int_wait`\ (3), :ref:`shmem_int_wait`\ (3)_until,
shmem_int4_wait\ (3), shmem_int4_wait\ (3)_until,
shmem_int8_wait\ (3), shmem_int8_wait\ (3)_until,
:ref:`shmem_long_wait`\ (3), :ref:`shmem_long_wait`\ (3)_until,
:ref:`shmem_longlong_wait`\ (3), :ref:`shmem_longlong_wait`\ (3)_until,
:ref:`shmem_short_wait`\ (3), :ref:`shmem_short_wait`\ (3)_until,
:ref:`shmem_wait`\ (3), :ref:`shmem_wait`\ (3)_until - Waits for a variable on the
local processing element (PE) to change


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_int_wait(volatile int *var, int value)

   void shmem_int_wait_until(volatile int *var, int cond, int value)

   void shmem_long_wait(volatile long *var, long value)

   void shmem_long_wait_until(volatile long *var, int cond, long value)

   void shmem_longlong_wait(volatile long long *var, long long value)

   void shmem_longlong_wait_until(volatile long long *var, int cond,
     long long value)

   void shmem_short_wait(volatile short *var, short value)

   void shmem_short_wait_until(volatile short *var, int cond,
     short value)

   void shmem_wait(volatile long *ivar, long cmp_value)

   void shmem_wait_until(volatile long *ivar, int cmp, long value)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   CALL SHMEM_INT4_WAIT(ivar, cmp_value)

   CALL SHMEM_INT4_WAIT_UNTIL(ivar, cmp, cmp_value)

   CALL SHMEM_INT8_WAIT(ivar, cmp_value)

   CALL SHMEM_INT8_WAIT_UNTIL(ivar, cmp, cmp_value)

   CALL SHMEM_WAIT(ivar, cmp_value)

   CALL SHMEM_WAIT_UNTIL(ivar, cmp, cmp_value)


DESCRIPTION
-----------

:ref:`shmem_wait` and :ref:`shmem_wait_until` wait for **ivar** to be changed by a
remote write or atomic swap issued by a different processor. These
routines can be used for point-to- point directed synchronization. A
call to :ref:`shmem_wait` does not return until some other processor writes a
value, not equal to cmp_value, into **ivar** on the waiting processor. A
call to :ref:`shmem_wait_until` does not return until some other processor
changes **ivar** to satisfy the condition implied by cmp and cmp_value.
This mechanism is useful when a processor needs to tell another
processor that it has completed some action.

The arguments are as follows:

target
   The remotely accessible integer data object to be updated on the
   remote PE. If you are using C/C++, the type of target should match
   that implied in the SYNOPSIS section. If you are using the Fortran
   compiler, it must be of type integer with an element size of 4 bytes
   for SHMEM_INT4_ADD and 8 bytes for SHMEM_INT8_ADD.

value
   The value to be atomically added to target. If you are using C/C++,
   the type of value should match that implied in the SYNOPSIS section.
   If you are using Fortran, it must be of type integer with an element
   size of target.

pe
   An integer that indicates the PE number upon which target is to be
   updated. If you are using Fortran, it must be a default integer
   value.

ivar
   A remotely accessible integer variable that is being updated by
   another PE. If you are using C/C++, the type of ivar should match
   that implied in the SYNOPSIS section. If you are using Fortran, ivar
   must be a specific sized integer type according to the function being
   called, as follows:

   :ref:`shmem_wait`, :ref:`shmem_wait_until`:** default INTEGER

   shmem_int4_wait, shmem_int4_wait_until:** INTEGER*4

   shmem_int8_wait, shmem_int8_wait_until:** INTEGER*8

cmp
   The compare operator that compares ivar with cmp_value. cmp must be
   of type integer. If you are using Fortran, it must be of default
   kind. If you are using C/C++, the type of cmp should match that
   implied in the SYNOPSIS section. The following cmp values are
   supported:

   SHMEM_CMP_EQ
      Equal

   SHMEM_CMP_NE
      Not equal

   SHMEM_CMP_GT
      Greater than

   SHMEM_CMP_LE
      Less than or equal to

   SHMEM_CMP_LT
      Less than

   SHMEM_CMP_GE
      Greater than or equal to

cmp_value
   cmp_value must be of type integer. If you are using C/C++, the type
   of cmp_value should match thatimplied in the SYNOPSIS section. If you
   are using Fortran, cmp_value must be an integer of the same size and
   kind as ivar. The :ref:`shmem_wait` routines return when ivar is no longer
   equal to cmp_value. The :ref:`shmem_wait_until` routines return when the
   compare condition is true. The compare condition is defined by the
   ivar argument compared with the cmp_value using the comparison
   operator, cmp.


EXAMPLES
--------

**Example 1:** The following call returns when variable ivar is not
equal to 100:

::

   INTEGER*8 IVAR

   CALL SHMEM_INT8_WAIT(IVAR, INT8(100))

**Example 2:** The following call to SHMEM_INT8_WAIT_UNTIL is equivalent
to the call to SHMEM_INT8_WAIT in example 1:

::

   INTEGER*8 IVAR

   CALL SHMEM_INT8_WAIT_UNTIL(IVAR, SHMEM_CMP_NE, INT8(100))

**Example 3:** The following C/C++ call waits until the sign bit in ivar
is set by a transfer from a remote PE:

::

   int ivar;

   shmem_int_wait_until(&ivar, SHMEM_CMP_LT, 0);

**Example 4:** The following Fortran example is in the context of a
subroutine:

::

   SUBROUTINE EXAMPLE()
     INTEGER FLAG_VAR
     COMMON/FLAG/FLAG_VAR
     . . .
     FLAG_VAR = FLAG_VALUE ! initialize the event variable
     . . .
     IF (FLAG_VAR .EQ. FLAG_VALUE) THEN
       CALL SHMEM_WAIT(FLAG_VAR, FLAG_VALUE)
     ENDIF
     FLAG_VAR = FLAG_VALUE ! reset the event variable for next time
     . . .
   END


.. seealso::
   *intro_shmem*\ (3) *shmem_put*\ (3)
