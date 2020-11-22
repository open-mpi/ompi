.. _shmem_alltoall32:


shmem_alltoall32
================

.. include_body

:ref:`shmem_alltoall32`\ (3), :ref:`shmem_alltoall64`\ (3),
:ref:`shmem_alltoalls32`\ (3), :ref:`shmem_alltoalls64`\ (3) - collective routine
where each PE exchanges a fixed amount of data with all other PEs in the
Active set


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_alltoall32(void *target, const void *source,
     size_t nelems, int PE_start, int logPE_stride,
     int PE_size, long *pSync)

   void shmem_alltoall64(void *target, const void *source,
     size_t nelems, int PE_start, int logPE_stride,
     int PE_size, long *pSync)

   void shmem_alltoalls32(void *target, const void *source,
     ptrdiff_t dst, ptrdiff_t sst,
     size_t nelems, int PE_start, int logPE_stride,
     int PE_size, long *pSync)

   void shmem_alltoalls64(void *target, const void *source,
     ptrdiff_t dst, ptrdiff_t sst,
     size_t nelems, int PE_start, int logPE_stride,
     int PE_size, long *pSync)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER dst, sst, nelems, PE_root, PE_start, logPE_stride, PE_size
   INTEGER pSync(SHMEM_ALLTOALL_SYNC_SIZE)

   CALL SHMEM_ALLTOALL32(target, source, nelems,
   & PE_start, logPE_stride, PE_size, fIpSync)

   CALL SHMEM_ALLTOALL64(target, source, nelems,
   & PE_start, logPE_stride, PE_size, pSync)

   CALL SHMEM_ALLTOALLS32(target, source, dst, sst, nelems,
   & PE_start, logPE_stride, PE_size, pSync)

   CALL SHMEM_ALLTOALLS64(target, source, dst, sst, nelems,
   & PE_start, logPE_stride, PE_size, pSync)


DESCRIPTION
-----------

The shmem_alltoalls routines are collective routines. Each PE in the
Active set exchanges nelems strided data elements of size 32 bits (for
shmem_alltoalls32) or 64 bits (for shmem_alltoalls64) with all other PEs
in the set. Both strides, dst and sst, must be greater than or equal to
1. The sst*jth block sent from PE i to PE j is placed in the dst*ith
block of the dest data object on PE j. As with all OpenSHMEM collective
routines, these routines assume that only PEs in the Active set call the
routine. If a PE not in the Active set calls an OpenSHMEM collective
routine, undefined behavior results. The values of arguments dst, sst,
nelems, PE_start, logPE_stride, and PE_size must be equal on all PEs in
the Active set. The same dest and source data objects, and the same
pSync work array must be passed to all PEs in the Active set. Before any
PE calls to a shmem_alltoalls routine, the following conditions must
exist (synchronization via a barrier or some other method is often
needed to ensure this): The pSync array on all PEs in the Active set is
not still in use from a prior call to a shmem_alltoalls routine. The
dest data object on all PEs in the Active set is ready to accept the
shmem_alltoalls data. Upon return from a shmem_alltoalls routine, the
following is true for the local PE: Its dest symmetric data object is
completely updated and the data has been copied out of the source data
object. The values in the pSync array are restored to the original
values.

The arguments are as follows:

A symmetric data object with one of the following data types:

   :ref:`shmem_alltoall32`: Any noncharacter type that
      has an element size of 32 bits. No Fortran derived types or C/C++
      structures are allowed.

   :ref:`shmem_alltoall64`: Any noncharacter type that has an element size
      of 64 bits. No Fortran derived types or C/C++ structures are
      allowed.

target A symmetric data object large enough to receive the combined
total of nelems elements from each PE in the Active set.

source
   A symmetric data object that contains nelems elements of data for
   each PE in the Active set, ordered according to destination PE.

dst
   The stride between consecutive elements of the dest data object. The
   stride is scaled by the element size. A value of 1 indicates
   contiguous data. dst must be of type ptrdiff_t. If you are using
   Fortran, it must be a default integer value.

sst
   The stride between consecutive elements of the source data object.
   The stride is scaled by the element size. A value of 1 indicates
   contiguous data. sst must be of type ptrdiff_t. If you are using
   Fortran, it must be a default integer value.

nelems
   The number of elements to exchange for each PE. nelems must be of
   type size_t for C/C++. If you are using Fortran, it must be a default
   integer value

PE_start
   The lowest virtual PE number of the active set of PEs. PE_start must
   be of type integer. If you are using Fortran, it must be a default
   integer value.

logPE_stride
   The log (base 2) of the stride between consecutive virtual PE numbers
   in the active set. log_PE_stride must be of type integer. If you are
   using Fortran, it must be a default integer value.

PE_size
   The number of PEs in the active set. PE_size must be of type integer.
   If you are using Fortran, it must be a default integer value.

pSync
   A symmetric work array. In C/C++, pSync must be of type long and size
   \_SHMEM_ALLTOALL_SYNC_SIZE. In Fortran, pSync must be of type integer
   and size SHMEM_ALLTOALL_SYNC_SIZE. Every element of this array must
   be initialized with the value \_SHMEM_SYNC_VALUE (in C/C++) or
   SHMEM_SYNC_VALUE (in Fortran) before any of the PEs in the active set
   enter shmem_barrier().

Upon return from a shmem_alltoalls routine, the following is true for
the local PE: Its dest symmetric data object is completely updated and
the data has been copied out of the source data object. The values in
the pSync array are restored to the original values.

The values of arguments PE_root, PE_start, logPE_stride, and PE_size
must be equal on all PEs in the active set. The same target and source
data objects and the same pSync work array must be passed to all PEs in
the active set.

Before any PE calls a alltoall routine, you must ensure that the
following conditions exist (synchronization via a barrier or some other
method is often needed to ensure this): The pSync array on all PEs in
the active set is not still in use from a prior call to a alltoall
routine. The target array on all PEs in the active set is ready to
accept the alltoall data.


NOTES
-----

The terms collective and symmetric are defined in *intro_shmem*\ (3).

All SHMEM alltoall routines restore pSync to its original contents.
Multiple calls to SHMEM routines that use the same pSync array do not
require that pSync be reinitialized after the first call.

You must ensure the that the pSync array is not being updated by any PE
in the active set while any of the PEs participates in processing of a
SHMEM broadcast routine. Be careful to avoid these situations: If the
pSync array is initialized at run time, some type of synchronization is
needed to ensure that all PEs in the working set have initialized pSync
before any of them enter a SHMEM routine called with the pSync
synchronization array. A pSync array may be reused on a subsequent SHMEM
broadcast routine only if none of the PEs in the active set are still
processing a prior SHMEM alltoall routine call that used the same pSync
array. In general, this can be ensured only by doing some type of
synchronization. However, in the special case of SHMEM routines being
called with the same active set, you can allocate two pSync arrays and
alternate between them on successive calls.


EXAMPLES
--------

C/C++ example:

.. code-block:: c++

   #include <shmem.h>
   #include <stdio.h>

   long pSync[SHMEM_ALLTOALL_SYNC_SIZE];
   int main(void)
   {
   int64_t *source, *dest;
   int i, count, pe;
   shmem_init();
   count = 2;
   dest = (int64_t*) shmem_malloc(count * shmem_n_pes() * sizeof(int64_t));
   source = (int64_t*) shmem_malloc(count * shmem_n_pes() * sizeof(int64_t));
   /* assign source values */
   for (pe=0; pe <shmem_n_pes(); pe++){
   for (i=0; i<count; i++){
   source[(pe*count)+i] = shmem_my_pe() + pe;
   dest[(pe*count)+i] = 9999;
   }
   }
   for (i=0; i< SHMEM_ALLTOALLS_SYNC_SIZE; i++) {
   pSync[i] = SHMEM_SYNC_VALUE;
   }
   /* wait for all PEs to initialize pSync */
   shmem_barrier_all();
   /* alltoalls on all PES */
   shmem_alltoalls64(dest, source, 1, 1, count, 0, 0, shmem_n_pes(), pSync);
   /* verify results */
   for (pe=0; pe<shmem_n_pes(); pe++) {
   for (i=0; i<count; i++){
   if (dest[(pe*count)+i] != shmem_my_pe() + pe) {
   printf("[%d] ERROR: dest[%d]=%ld, should be %d,
   shmem_my_pe(),(pe*count)+i,dest[(pe*count)+i],
   shmem_n_pes() + pe);
   }
   }
   }
   shmem_barrier_all();
   shmem_free(dest);
   shmem_free(source);
   shmem_finalize();
   return 0;
   }


.. seealso::
   *intro_shmem*\ (3)
