.. _shmem_my_pe:


shmem_my_pe
===========

.. include_body

:ref:`shmem_my_pe`, my_pe, \_my_pe - Returns the virtual PE number of the
calling PE.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   int shmem_my_pe (void)
   int my_pe (void)

Fortran:

.. code-block:: fortran

   include 'mpp/shmem.fh'
   I = SHMEM_MY_PE ()
   I = MY_PE ()


DESCRIPTION
-----------

my_pe() or shmem_my_pe() return the processing element (PE) number of
the calling PE. It accepts no arguments. The result is an integer
between 0 and npes - 1, where npes is the total number of PEs executing
the current program.


.. seealso::
   *intro_shmem*\ (3) *shmem_n_pes*\ (3) *shmem_init*\ (3)
