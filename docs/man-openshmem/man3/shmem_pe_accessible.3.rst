.. _shmem_pe_accessible:


shmem_pe_accessible
===================

.. include_body

:ref:`shmem_pe_accessible` - Determines whether a processing element (PE) is
accessible via SHMEM data transfer operations.


SYNOPSIS
--------

C:

.. code-block:: c

   #include <mpp/shmem.h>

   int shmem_pe_accessible(int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   LOGICAL LOG, SHMEM_PE_ACCESSIBLE
   INTEGER pe

   LOG = SHMEM_PE_ACCESSIBLE(pe)


DESCRIPTION
-----------

:ref:`shmem_pe_accessible` returns a value that indicates whether the calling
PE is able to perform OpenSHMEM communication operations with the remote
PE.


RETURN VALUES
-------------

C/C++
   The return value is 1 if the specified PE is a valid remote PE for
   SHMEM functions; otherwise,it is 0.

Fortran
   The return value is .TRUE. if the specified PE is a valid remote PE
   for SHMEM functions; otherwise, it is .FALSE..


.. seealso::
   *intro_shmem*\ (3) *shmem_addr_accessible*\ (3)
