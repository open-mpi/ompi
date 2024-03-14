.. _shmem_ptr:


shmem_ptr
=========

.. include_body

:ref:`shmem_ptr`\ (3) - Returns a pointer to a data object on a specified
processing element (PE).


SYNOPSIS
--------

C or C++:

.. code-block:: c++

   #include <mpp/shmem.h>

   void *shmem_ptr(const void *target, int pe)

Fortran:

.. code-block:: fortran

   INCLUDE "mpp/shmem.fh"

   POINTER (PTR, POINTEE)
   INTEGER pe

   PTR = SHMEM_PTR(target, pe)


DESCRIPTION
-----------

The :ref:`shmem_ptr` routine returns an address that can be used to directly
reference **target** on the remote PE **pe**. With this address we can
perform ordinary loads and stores to the remote address.

When a sequence of loads (gets) and stores (puts) to a data object on a
remote PE does not match the access pattern provided in a SHMEM data
transfer routine like :ref:`shmem_put32`\ (3) or shmem_real_iget\ (3), the
:ref:`shmem_ptr` function can provide an efficient means to accomplish the
communication.

The arguments are as follows:

target
   The symmetric data object to be referenced.

pe
   An integer that indicates the PE number on which target is to be
   accessed. If you are using Fortran, it must be a default integer
   value.


EXAMPLES
--------

This Fortran program calls :ref:`shmem_ptr` and then PE 0 writes to the BIGD
array on PE 1:

::

   PROGRAM REMOTEWRITE
     INCLUDE 'mpp/shmem.fh'

     INTEGER BIGD(100)
     SAVE BIGD
     INTEGER POINTEE(*)

     POINTER (PTR,POINTEE)
     CALL START_PES(0)
     IF (MY_PE() .EQ. 0) THEN
                                ! initialize PE 1's BIGD array
       PTR = SHMEM_PTR(BIGD, 1) ! get address of PE 1's BIGD
                                ! array
       DO I=1,100
         POINTEE(I) = I
       ENDDO
     ENDIF
     CALL SHMEM_BARRIER_ALL
     IF (MY_PE() .EQ. 1) THEN
       PRINT *, 'BIGD on PE 1 is: '
       PRINT *, BIGD
     ENDIF
   END

This is the equivalent program written in C:

.. code-block:: c

   #include <mpp/shmem.h>

   main()
   {
     static int bigd[100];
     int *ptr;
     int i;

     shmem_init();
     if (shmem_my_pe() == 0) {
     /* initialize PE 1's bigd array */
       ptr = shmem_ptr(bigd, 1);
       for (i=0; i<100; i++)
         *ptr++ = i+1;
     }
     shmem_barrier_all();
     if (shmem_my_pe() == 1) {
       printf("bigd on PE 1 is:\n");
       for (i=0; i<100; i++)
         printf(" %d\n",bigd[i]);
       printf("\n");
     }
   }


NOTES
-----

The :ref:`shmem_ptr` function is available only on systems where ordinary
memory loads and stores are used to implement SHMEM put and get
operations.


RETURN VALUES
-------------

:ref:`shmem_ptr` returns a pointer to the data object on the specified remote
PE. If target is not remotely accessible, a NULL pointer is returned.


.. seealso::
   *intro_shmem*\ (3) *shmem_put*\ (3) *shmem_get*\ (3)
