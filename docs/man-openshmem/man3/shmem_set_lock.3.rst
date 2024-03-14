.. _shmem_set_lock:


shmem_set_lock
==============

.. include_body

:ref:`shmem_set_lock`\ (3), :ref:`shmem_clear_lock`\ (3), :ref:`shmem_test_lock`\ (3) -
Releases, locks, and tests a mutual exclusion memory lock.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_clear_lock(volatile long *lock)

   void shmem_set_lock(volatile long *lock)

   int shmem_test_lock(volatile long *lock)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER lock, SHMEM_TEST_LOCK

   CALL SHMEM_CLEAR_LOCK(lock)

   CALL SHMEM_SET_LOCK(lock)

   I = SHMEM_TEST_LOCK(lock)


DESCRIPTION
-----------

The :ref:`shmem_set_lock` routine sets a mutual exclusion lock after waiting
for the lock to be freed by any other PE currently holding the lock.
Waiting PEs are assured of getting the lock in a first-come,
first-served manner.

The :ref:`shmem_clear_lock` routine releases a lock previously set by
:ref:`shmem_set_lock` after ensuring that all local and remote stores initiated
in the critical region are complete.

The :ref:`shmem_test_lock` function sets a mutual exclusion lock only if it is
currently cleared. By using this function, a PE can avoid blocking on a
set lock. If the lock is currently set, the function returns without
waiting. These routines are appropriate for protecting a critical region
from simultaneous update by multiple PEs. They accept the following
arguments:

lock
   A symmetric data object that is a scalar variable or an array of
   length 1. This data object must be set to 0 on all processing
   elements (PEs) prior to the first use. lock must be of type integer.
   If you are using Fortran, it must be of default kind.


NOTES
-----

The term symmetric data object is defined on *intro_shmem*\ (3).


RETURN VALUES
-------------

The :ref:`shmem_test_lock` function returns 0 if the lock was originally
cleared and this call was able to set the lock. A value of 1 is returned
if the lock had been set and the call returned without waiting to set
the lock.


.. seealso::
   *intro_shmem*\ (3)
