.. _shmem_broadcast32:


shmem_broadcast32
=================

.. include_body

shmem_broadcast4\ (3), shmem_broadcast8\ (3),
:ref:`shmem_broadcast32`\ (3), :ref:`shmem_broadcast64`\ (3) - Copy a data object
from a designated PE to a target location on all other PEs of the active
set.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_broadcast32(void *target, const void *source,
     size_t nelems, int PE_root, int PE_start, int logPE_stride,
     int PE_size, long *pSync)

   void shmem_broadcast64(void *target, const void *source,
     size_t nelems, int PE_root, int PE_start, int logPE_stride,
     int PE_size, long *pSync)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   INTEGER nelems, PE_root, PE_start, logPE_stride, PE_size
   INTEGER pSync(SHMEM_BCAST_SYNC_SIZE)

   CALL SHMEM_BROADCAST4(target, source, nelems, PE_root,
   & PE_start, logPE_stride, PE_size, fIpSync)

   CALL SHMEM_BROADCAST8(target, source, nelems, PE_root,
   & PE_start, logPE_stride, PE_size, pSync)

   CALL SHMEM_BROADCAST32(target, source, nelems,
   & PE_root, PE_start, logPE_stride, PE_size, pSync)

   CALL SHMEM_BROADCAST64(target, source, nelems,
   & PE_root, PE_start, logPE_stride, PE_size, pSync)


DESCRIPTION
-----------

The broadcast routines write the data at address source of the PE
specified by **PE_root** to address **target** on all other PEs in the
active set. The active set of PEs is defined by the triplet
**PE_start**, **logPE_stride** and **PE_size**. The data is not copied
to the target address on the PE specified by **PE_root**. Before
returning, the broadcast routines ensure that the elements of the pSync
array are restored to their initial values.

As with all SHMEM collective routines, each of these routines assumes
that only PEs in the active set call the routine. If a PE not in the
active set calls a SHMEM collective routine, undefined behavior results.

The arguments are as follows:

target
   A symmetric data object with one of the following data types:

   shmem_broadcast8, :ref:`shmem_broadcast64`: Any noncharacter type that
      has an element size of 64 bits. No Fortran derived types or C/C++
      structures are allowed.

   :ref:`shmem_broadcast32`: Any noncharacter type that has an element size
      of 32 bits. No Fortran derived types or C/C++ structures are
      allowed.

   shmem_broadcast4: Any noncharacter type that has an element size
      of 32 bits.

source
   A symmetric data object that can be of any data type that is
   permissible for the target argument.

nelems
   The number of elements in source. For :ref:`shmem_broadcast32` and
   shmem_broadcast4, this is the number of 32-bit halfwords. nelems must
   be of type integer. If you are using Fortran, it must be a default
   integer value.

PE_root
   Zero-based ordinal of the PE, with respect to the active set, from
   which the data is copied. Must be greater than or equal to 0 and less
   than PE_size. PE_root must be of type integer. If you are using
   Fortran, it must be a default integer value.

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
   \_SHMEM_BCAST_SYNC_SIZE. In Fortran, pSync must be of type integer
   and size SHMEM_BCAST_SYNC_SIZE. Every element of this array must be
   initialized with the value \_SHMEM_SYNC_VALUE (in C/C++) or
   SHMEM_SYNC_VALUE (in Fortran) before any of the PEs in the active set
   enter shmem_barrier().

The values of arguments PE_root, PE_start, logPE_stride, and PE_size
must be equal on all PEs in the active set. The same target and source
data objects and the same pSync work array must be passed to all PEs in
the active set.

Before any PE calls a broadcast routine, you must ensure that the
following conditions exist (synchronization via a barrier or some other
method is often needed to ensure this): The pSync array on all PEs in
the active set is not still in use from a prior call to a broadcast
routine. The target array on all PEs in the active set is ready to
accept the broadcast data.

Upon return from a broadcast routine, the following are true for the
local PE: If the current PE is not the root PE, the target data object
is updated. The values in the pSync array are restored to the original
values.


NOTES
-----

The terms collective and symmetric are defined in *intro_shmem*\ (3).

All SHMEM broadcast routines restore pSync to its original contents.
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
processing a prior SHMEM broadcast routine call that used the same pSync
array. In general, this can be ensured only by doing some type of
synchronization. However, in the special case of SHMEM routines being
called with the same active set, you can allocate two pSync arrays and
alternate between them on successive calls.


EXAMPLES
--------

In the following examples, the call to :ref:`shmem_broadcast64` copies source
on PE 4 to target on PEs 5, 6, and 7.

C/C++ example:

.. code-block:: c++

   for (i=0; i < _SHMEM_BCAST_SYNC_SIZE; i++) {
     pSync[i] = _SHMEM_SYNC_VALUE;
   }
   shmem_barrier_all(); /* Wait for all PEs to initialize pSync */
   shmem_broadcast64(target, source, nelems, 0, 4, 0, 4, pSync);

Fortran example:

.. code-block:: fortran

   INTEGER PSYNC(SHMEM_BCAST_SYNC_SIZE)
   INTEGER TARGET, SOURCE, NELEMS, PE_ROOT, PE_START,
   & LOGPE_STRIDE, PE_SIZE, PSYNC
   COMMON /COM/ TARGET, SOURCE
   DATA PSYNC /SHMEM_BCAST_SYNC_SIZE*SHMEM_SYNC_VALUE/

   CALL SHMEM_BROADCAST64(TARGET, SOURCE, NELEMS, 0, 4, 0, 4,
   & PSYNC)


.. seealso::
   *intro_shmem*\ (3)
