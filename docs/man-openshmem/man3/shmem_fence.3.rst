.. _shmem_fence:


shmem_fence
===========

.. include_body

:ref:`shmem_fence` - Provides a separate ordering on the sequence of puts
issued by this PE to each destination PE.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_fence(void)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   CALL SHMEM_FENCE


DESCRIPTION
-----------

The shmem_fence() routine provides an ordering on the put operations
issued by the calling PE prior to the call to shmem_fence() relative
to the put operations issued by the calling PE following the call to
shmem_fence(). It guarantees that all such prior put operations
issued to a particular destination PE are fully written to the symmetric
memory of that destination PE, before any such following put operations
to that same destination PE are written to the symmetric memory of that
destination PE. Note that the ordering is provided separately on the
sequences of puts from the calling PE to each distinct destination PE.
The shmem_quiet() routine should be used instead if ordering of puts
is required when multiple destination PEs are involved.


NOTES
-----

The :ref:`shmem_quiet` function should be called if ordering of puts is desired
when multiple remote PEs are involved.


.. seealso::
   *intro_shmem*\ (3)
