.. _shmem_collect32:


shmem_collect32
===============

.. include_body

shmem_collect4\ (3), shmem_collect8\ (3), :ref:`shmem_collect32`\ (3),
:ref:`shmem_collect64`\ (3), shmem_fcollect\ (3), shmem_fcollect4\
(3), shmem_fcollect8\ (3), :ref:`shmem_fcollect32`\ (3),
:ref:`shmem_fcollect64`\ (3) - Concatenates blocks of data from
multiple processing elements (PEs) to an array in every PE



SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_collect32(void *target, const void *source,
     size_t nelems, int PE_start, int logPE_stride, int PE_size,
     long *pSync)

   void shmem_collect64(void *target, const void *source,
     size_t nelems, int PE_start, int logPE_stride, int PE_size,
     long *pSync)

   void shmem_fcollect32(void *target, const void *source,
     size_t nelems, int PE_start, int logPE_stride, int PE_size,
     long *pSync)

   void shmem_fcollect64(void *target, const void *source,
     size_t nelems, int PE_start, int logPE_stride, int PE_size,
     long *pSync)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER nelems
   INTEGER PE_start, logPE_stride, PE_size
   INTEGER pSync(SHMEM_COLLECT_SYNC_SIZE)

   CALL SHMEM_COLLECT4(target, source, nelems, PE_start,
   & logPE_stride, PE_size, pSync)

   CALL SHMEM_COLLECT8(target, source, nelems, PE_start,
   & logPE_stride, PE_size, pSync)

   CALL SHMEM_FCOLLECT4(target, source, nelems, PE_start,
   & logPE_stride, PE_size, pSync)

   CALL SHMEM_FCOLLECT8(target, source, nelems, PE_start,
   & logPE_stride, PE_size, pSync)


DESCRIPTION
-----------

The shared memory (SHMEM) collect and fcollect routines concatenate
nelems 64-bit or 32-bit data items from the source array into the target
array, over the set of PEs defined by PE_start, log2PE_stride, and
PE_size, in processor number order. The resultant target array contains
the contribution from PE PE_start first, then the contribution from PE
PE_start + PE_stride second, and so on. The collected result is written
to the target array for all PEs in the active set.

The fcollect routines require that nelems be the same value in all
participating PEs, while the collect routines allow nelems to vary from
PE to PE.

The resulting target array is as follows:

::

   ----------------------------------------------------------
      source(1..nelems)
          from PE (PE_start + 0 * (2**logPE_stride))
   ----------------------------------------------------------
      source(1..nelems)
          from PE (PE_start + 1 * (2**logPE_stride))
   ----------------------------------------------------------
      ...
   ----------------------------------------------------------
      source(1..nelems) from
          PE (PE_start + (PE_size - 1) * (2**logPE_stride))
   ----------------------------------------------------------

As with all SHMEM collective routines, each of these routines assumes
that only PEs in the active set call the routine. If a PE not in the
active set calls a SHMEM collective routine, undefined behavior results.

The arguments are as follows:

target
   A symmetric array. The target argument must be large enough to accept
   the concatenation of the source arrays on all PEs. The data types are
   as follows:

   [shmem_collect8, :ref:`shmem_collect64`, shmem_fcollect8, and
      shmem_fcollect64] any data type with an element size of 64 bits.
      Fortran derived types, Fortran character type, and C/C++
      structures are not permitted.

   [shmem_collect4, :ref:`shmem_collect32`, shmem_fcollect4, and
      shmem_fcollect32] any data type with an element size of 32 bits.
      Fortran derived types, Fortran character type, and C/C++
      structures are not permitted.

source
   A symmetric data object that can be of any type permissible for the
   target argument.

nelems
   The number of elements in the source array. nelems must be of type
   integer. If you are using Fortran, it must be a default integer
   value.

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
   \_SHMEM_COLLECT_SYNC_SIZE. In Fortran, pSync must be of type integer
   and size SHMEM_COLLECT_SYNC_SIZE. If you are using Fortran, it must
   be a default integer value. Every element of this array must be
   initialized with the value \_SHMEM_SYNC_VALUE in C/C++ or
   SHMEM_SYNC_VALUE in Fortran before any of the PEs in the active set
   enter shmem_barrier().

The values of arguments PE_start, logPE_stride, and PE_size must be
equal on all PEs in the active set. The same target and source arrays
and the same pSync work array must be passed to all PEs in the active
set.

Upon return from a collective routine, the following are true for the
local PE: The target array is updated. The values in the pSync array are
restored to the original values.


NOTES
-----

The terms collective and symmetric are defined in *intro_shmem*\ (3).
All SHMEM collective routines reset the values in pSync before they
return, so a particular pSync buffer need only be initialized the first
time it is used.

You must ensure that the pSync array is not being updated on any PE in
the active set while any of the PEs participate in processing of a SHMEM
collective routine. Be careful to avoid these situations: If the pSync
array is initialized at run time, some type of synchronization is needed
to ensure that all PEs in the working set have initialized pSync before
any of them enter a SHMEM routine called with the pSync synchronization
array. A pSync array can be reused on a subsequent SHMEM collective
routine only if none of the PEs in the active set are still processing a
prior SHMEM collective routine call that used the same pSync array. In
general, this may be ensured only by doing some type of synchronization.
However, in the special case of SHMEM routines being called with the
same active set, you can allocate two pSync arrays and alternate between
them on successive calls.

The collective routines operate on active PE sets that have a
non-power-of-two PE_size with some performance degradation. They operate
with no performance degradation when nelems is a non-power-of-two value.


EXAMPLES
--------

C/C++:

.. code-block:: c++

   for (i=0; i < _SHMEM_COLLECT_SYNC_SIZE; i++) {
     pSync[i] = _SHMEM_SYNC_VALUE;
   }
   shmem_barrier_all(); /* Wait for all PEs to initialize pSync */
   shmem_collect32(target, source, 64, pe_start, logPE_stride,
      pe_size, pSync);

Fortran:

.. code-block:: fortran

   INTEGER PSYNC(SHMEM_COLLECT_SYNC_SIZE)
   DATA PSYNC /SHMEM_COLLECT_SYNC_SIZE*SHMEM_SYNC_VALUE/

   CALL SHMEM_COLLECT4(TARGET, SOURCE, 64, PE_START,
   & LOGPE_STRIDE, PE_SIZE, PSYNC)


.. seealso::
   *intro_shmem*\ (3)
