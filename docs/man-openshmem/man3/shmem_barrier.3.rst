.. _shmem_barrier:


shmem_barrier
=============

.. include_body

:ref:`shmem_barrier` - Performs a barrier operation on a subset of processing
elements (PEs).


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_barrier(int PE_start, int logPE_stride, int PE_size,
     long *pSync)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER PE_start, logPE_stride, PE_size
   INTEGER pSync(SHMEM_BARRIER_SYNC_SIZE)

   CALL SHMEM_BARRIER(PE_start, logPE_stride, PE_size, pSync)


DESCRIPTION
-----------

The :ref:`shmem_barrier` routine does not return until the subset of PEs
specified by **PE_start**, **logPE_stride** and **PE_size**, has entered
this routine at the same point of the execution path.

As with all SHMEM collective routines, each of these routines assumes
that only PEs in the active set call the routine. If a PE not in the
active set calls a SHMEM collective routine, undefined behavior results.

The arguments are as follows:

PE_start
   The lowest virtual PE number of the active set of PEs. PE_start must
   be of type integer. If you are using Fortran, it must be a default
   integer value.

logPE_stride
   The log (base 2) of the stride between consecutive virtual PE numbers
   in the active set. logPE_stride must be of type integer. If you are
   using Fortran, it must be a default integer value.

PE_size
   The number of PEs in the active set. PE_size must be of type integer.
   If you are using Fortran, it must be a default integer value.

pSync
   A symmetric work array. In C/C++, pSync must be of type int and size
   \_SHMEM_BARRIER_SYNC_SIZE. In Fortran, pSync must be of type integer
   and size SHMEM_BARRIER_SYNC_SIZE. If you are using Fortran, it must
   be a default integer type. Every element of this array must be
   initialized to 0 before any of the PEs in the active set enter
   :ref:`shmem_barrier` the first time.

The values of arguments PE_start, logPE_stride, and PE_size must be
equal on all PEs in the active set. The same work array must be passed
in pSync to all PEs in the active set.

:ref:`shmem_barrier` ensures that all previously issued local stores and
previously issued remote memory updates done by any of the PEs in the
active set (by using SHMEM calls, for example shmem_put\ (3)) are
complete before returning.

The same pSync array may be reused on consecutive calls to :ref:`shmem_barrier`
if the same active PE set is used.


NOTES
-----

The term symmetric is defined in *intro_shmem*\ (3).

If the pSync array is initialized at run time, be sure to use some type
of synchronization, for example, a call to :ref:`shmem_barrier_all`\ (3),
before calling :ref:`shmem_barrier` for the first time.

If the active set does not change, :ref:`shmem_barrier` can be called
repeatedly with the same pSync array. No additional synchronization
beyond that implied by :ref:`shmem_barrier` itself is necessary in this case.


EXAMPLES
--------

C/C++ example:

.. code-block:: c++

   shmem_barrier(PE_start, logPE_stride, size, pSync);

Fortran example:

.. code-block:: fortran

   INTEGER PSYNC(SHMEM_BARRIER_SYNC_SIZE)
   INTEGER PE_START, LOGPE_STRIDE, PE_SIZE, PSYNC
   DATA PSYNC /SHMEM_BARRIER_SYNC_SIZE*0/

   CALL SHMEM_BARRIER(PE_START, LOGPE_STRIDE, PE_SIZE, PSYNC)


.. seealso::
   *intro_shmem*\ (3) *shmem_barrier_all*\ (3)
