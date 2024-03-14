.. _shmem_addr_accessible:


shmem_addr_accessible
=====================

.. include_body

:ref:`shmem_addr_accessible` - Indicates if an address is accessible via
OpenSHMEM operations from the specified remote PE.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   int shmem_addr_accessible(const void *addr, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   LOGICAL LOG, SHMEM_ADDR_ACCESSIBLE
   INTEGER pe

   LOG = SHMEM_ADDR_ACCESSIBLE(addr, pe)


DESCRIPTION
-----------

:ref:`shmem_addr_accessible` is a query function that indicates whether a local
address is accessible via SHMEM operations from the specified remote PE.

This function verifies that the remote PE is accessible via SHMEM data
transfer functions from the local PE, and that the specified address is
in a symmetric data segment with respect to the remote PE.


RETURN VALUES
-------------

C: The return value is 1 if addr is a symmetric data object and
accessible via SHMEM operations from the specified remote PE; otherwise,
it is 0.

Fortran: The return value is .TRUE. if addr is a symmetric data object
and accessible via SHMEM operations from the specified remote PE;
otherwise, it is .FALSE..


.. seealso::
   *intro_shmem*\ (3) *shmem_pe_accessible*\ (3)
