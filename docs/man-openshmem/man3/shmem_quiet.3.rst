.. _shmem_quiet:


shmem_quiet
===========

.. include_body

:ref:`shmem_quiet`\ (3) - Waits for completion of all outstanding remote
writes issued by a processing element (PE).


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_quiet(void)

Fortran:

.. code-block:: fortran

   CALL SHMEM_QUIET


DESCRIPTION
-----------

:ref:`shmem_quiet` ensures ordering of put (remote write) operations. All put
operations issued to any processing element (PE) prior to the call to
:ref:`shmem_quiet` are guaranteed to be visible to all other PEs no later than
any subsequent memory load or store, remote put or get, or
synchronization operations that follow the call to :ref:`shmem_quiet`.


NOTES
-----

| :ref:`shmem_quiet` is most useful as a way of ensuring ordering of delivery
  of several put operations. For example, you might use :ref:`shmem_quiet` to
  await delivery of a block of data before issuing another put, which
  sets a completion flag on another PE.
| :ref:`shmem_quiet` is not usually needed if :ref:`shmem_barrier_all`\ (3) or
  :ref:`shmem_barrier`\ (3) are called. The barrier routines all wait for the
  completion of outstanding remote writes (puts).


EXAMPLES
--------

::

   PROGRAM COMPFLAG
     INCLUDE "mpp/shmem.fh"

     INTEGER FLAG_VAR, ARRAY(100), RECEIVER, SENDER
     COMMON/FLAG/FLAG_VAR
     COMMON/DATA/ARRAY
     INTRINSIC MY_PE

     FLAG_VAR = 0
     CALL SHMEM_BARRIER_ALL ! wait for FLAG_VAR to be initialized
     SENDER = 0                        ! PE 0 sends the data
     RECEIVER = 1                      ! PE 1 receives the data

     IF (MY_PE() .EQ. 0) THEN
       ARRAY = 33
       CALL SHMEM_PUT(ARRAY, ARRAY, 100, RECEIVER) ! start sending data
       CALL SHMEM_QUIET                ! wait for delivery
       CALL SHMEM_PUT(FLAG_VAR, 1, 1, RECEIVER) ! send completion flag
     ELSE IF (MY_PE() .EQ. RECEIVER) THEN
       CALL SHMEM_UDCFLUSH
       CALL SHMEM_WAIT(FLAG_VAR, 0)
       PRINT *,ARRAY                       ! ARRAY has been delivered
     ENDIF
   END


.. seealso::
   *intro_shmem*\ (3) :ref:`shmem_barrier`\ (3) :ref:`shmem_barrier_all`\ (3)
   *shmem_fence*\ (3) *shmem_put*\ (3) *shmem_wait*\ (3)
