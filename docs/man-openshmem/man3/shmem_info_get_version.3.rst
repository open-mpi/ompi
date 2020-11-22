.. _shmem_info_get_version:


shmem_info_get_version
======================

.. include_body

:ref:`shmem_info_get_version` - Returns the major and minor version of the
library implementation.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_info_get_version(int *major, int *minor)

Fortran:

.. code-block:: fortran

   include 'mpp/shmem.fh'
   SHMEM_INFO_GET_VERSION(MAJOR, MINOR)
   INTEGER MAJOR, MINOR


DESCRIPTION
-----------

shmem_info_get_version() This routine returns the major and minor
version of the OpenSHMEM standard in use. For a given library
implementation, the major and minor version returned by these calls is
consistent with the compile-time constants, SHMEM_MAJOR_VERSION and
SHMEM_MINOR_VERSION, defined in its shmem.h. The valid major version
value is 1, and the valid minor version value is 2.


.. seealso::
   *intro_shmem*\ (3) *shmem_my_pe*\ (3) *shmem_init*\ (3)
