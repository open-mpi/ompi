.. _shmem_finalize:


shmem_finalize
==============

.. include_body

:ref:`shmem_finalize` - A collective operation that releases resources used by
the OpenSHMEM library. This only terminates the Open-SHMEM portion of a
program, not the entire program.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_finalize(void)

Fortran:

.. code-block:: fortran

   include 'mpp/shmem.fh'
   CALL SHMEM_FINALIZE


DESCRIPTION
-----------

:ref:`shmem_finalize` is a collective operation that ends the OpenSHMEM portion
of a program previously initialized by :ref:`shmem_init` and releases resources
used by the OpenSHMEM library. This collective operation requires all
PEs to participate in the call. There is an implicit global barrier in
:ref:`shmem_finalize` so that pending communication is completed, and no
resources can be released until all PEs have entered :ref:`shmem_finalize`.
:ref:`shmem_finalize` must be the last OpenSHMEM library call encountered in
the OpenSHMEM portion of a program. A call to :ref:`shmem_finalize` will
release any resources initialized by a corresponding call to :ref:`shmem_init`.
All processes and threads that represent the PEs will still exist after
the call to :ref:`shmem_finalize` returns, but they will no longer have access
to any resources that have been released.


.. seealso::
   *intro_shmem*\ (3) *shmem_my_pe*\ (3) *shmem_init*\ (3)
