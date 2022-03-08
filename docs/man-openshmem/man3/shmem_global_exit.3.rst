.. _shmem_global_exit:


shmem_global_exit
=================

.. include_body

:ref:`shmem_global_exit` - A routine that allows any PE to force termination of
an entire program.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void shmem_global_exit(int status)

Fortran:

.. code-block:: fortran

   include 'mpp/shmem.fh'
   INTEGER STATUS
   CALL SHMEM_GLOBAL_EXIT(status)


DESCRIPTION
-----------

shmem_global_exit() :ref:`shmem_global_exit` is a non-collective routine that
allows any one PE to force termination of an Open- SHMEM program for all
PEs, passing an exit status to the execution environment. This routine
terminates the entire program, not just the OpenSHMEM portion. When any
PE calls :ref:`shmem_global_exit`, it results in the immediate notification to
all PEs to terminate. :ref:`shmem_global_exit` flushes I/O and releases
resources in accordance with C/C++/Fortran language requirements for
normal program termination. If more than one PE calls :ref:`shmem_global_exit`,
then the exit status returned to the environment shall be one of the
values passed to :ref:`shmem_global_exit` as the status argument. There is no
return to the caller of :ref:`shmem_global_exit`; control is returned from the
OpenSHMEM program to the execution environment for all PEs.


.. seealso::
   *intro_shmem*\ (3) *shmem_my_pe*\ (3) *shmem_init*\ (3)
