.. _shmem_info_get_name:


shmem_info_get_name
===================

.. include_body

:ref:`shmem_info_get_name` - This routine returns the vendor defined character
string.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_info_get_name(char *name)

Fortran:

.. code-block:: fortran

   include 'mpp/shmem.fh'
   SHMEM_INFO_GET_NAME(NAME)
   CHARACTER *(*)NAME


DESCRIPTION
-----------

shmem_info_get_name() This routine returns the vendor defined character
string of size defined by the constant SHMEM_MAX_NAME_LEN. The program
calling this function prepares the memory of size SHMEM_MAX_NAME_LEN,
and the implementation copies the string of size at most
SHMEM_MAX_NAME_LEN. In C, the string is terminated by a null character.
In Fortran, the string of size less than SHMEM_MAX_NAME_LEN is padded
with blank characters up to size SHMEM_MAX_NAME_LEN. The implementation
copying a string of size greater than SHMEM_MAX_NAME_LEN results in an
undefined behavior. Multiple invocations of the routine in an OpenSHMEM
program always return the same string. For a given library
implementation, the major and minor version returned by these calls is
consistent with the compile-time constants defined in its shmem.h.


.. seealso::
   *intro_shmem*\ (3) *shmem_my_pe*\ (3) *shmem_init*\ (3)
