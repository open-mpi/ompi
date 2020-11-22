.. _shmem_init:


shmem_init
==========

.. include_body

:ref:`shmem_init`, start_pes - Allocates a block of memory from the symmetric
heap.


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   void shmem_init(void)
   void start_pes(int npes)

Fortran:

.. code-block:: fortran

   CALL SHMEM_INIT()
   CALL START_PES(npes)


DESCRIPTION
-----------

The start_pes routine should be the first statement in a SHMEM parallel
program.

The start_pes routine accepts the following argument:

npes
   Unused. Should be set to 0.

This routine initializes the SHMEM API, therefore it must be called
before calling any other SHMEM routine. This routine is responsible
inter alia for setting up the symmetric heap on the calling PE, and the
creation of the virtual PE numbers. Upon successful return from this
routine, the calling PE will be able to communicate with and transfer
data to other PEs.

Multiple calls to this function are not allowed.

For an overview of programming with SHMEM communication routines,
example SHMEM programs, and instructions for compiling SHMEM programs,
see the *intro_shmem*\ (3) man page.


EXAMPLES
--------

This is a simple program that calls shmem_integer_put\ (3):

::

   PROGRAM PUT
     INCLUDE "mpp/shmem.fh"

     INTEGER TARG, SRC, RECEIVER, BAR
     COMMON /T/ TARG
     PARAMETER (RECEIVER=1)

     CALL SHMEM_INIT()
     IF (MY_PE() .EQ. 0) THEN
       SRC = 33
       CALL SHMEM_INTEGER_PUT(TARG, SRC, 1, RECEIVER)
     ENDIF
     CALL SHMEM_BARRIER_ALL ! SYNCHRONIZES SENDER AND RECEIVER
     IF (MY_PE() .EQ. RECEIVER) THEN
       PRINT *,'PE ', MY_PE(),' TARG=',TARG,' (expect 33)'
     ENDIF
   END


NOTES
-----

If the start_pes call is not the first statement in a program,
unexpected results may occur on some architectures.


.. seealso::
   *intro_shmem*\ (3) :ref:`shmem_barrier`\ (3) :ref:`shmem_barrier_all`\ (3)
   *shmem_put*\ (3) *my_pe*\ (3) *shmem_n_pes*\ (3)
