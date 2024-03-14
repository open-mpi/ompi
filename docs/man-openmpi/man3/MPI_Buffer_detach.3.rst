.. _mpi_buffer_detach:

MPI_Buffer_detach
=================

.. include_body

:ref:`MPI_Buffer_detach` |mdash| Removes an existing buffer (for use in in :ref:`MPI_Bsend`,
etc.)

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Buffer_detach(void *buf, int *size)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_BUFFER_DETACH(BUF, SIZE, IERROR)
       <type>  BUF(*)
       INTEGER SIZE, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Buffer_detach(buffer_addr, size, ierror)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY
       TYPE(C_PTR), INTENT(OUT) :: buffer_addr
       INTEGER, INTENT(OUT) :: size
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

OUTPUT PARAMETERS
-----------------

* ``buf`` : Initial buffer address (choice).
* ``size`` : Buffer size, in bytes (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Detach the buffer currently associated with MPI. The call returns the
address and the size of the detached buffer. This operation will block
until all messages currently in the buffer have been transmitted. Upon
return of this function, the user may reuse or deallocate the space
taken by the buffer.

Example: Calls to attach and detach buffers.

.. code-block:: c

   #define BUFFSIZE 10000

   int size char *buff;
   MPI_Buffer_attach( malloc(BUFFSIZE), BUFFSIZE);

   // a buffer of 10000 bytes can now be used by MPI_Bsend

   MPI_Buffer_detach( &buff, &size); // Buffer size reduced to zero
   MPI_Buffer_attach( buff, size); // Buffer of 10000 bytes available again

NOTES
-----

The reason that :ref:`MPI_Buffer_detach` returns the address and size of the
buffer being detached is to allow nested libraries to replace and
restore the buffer. For example, consider

.. code-block:: c
   
   int size, mysize, idummy;
   void *ptr, *myptr, *dummy;
   MPI_Buffer_detach( &ptr, &size );
   MPI_Buffer_attach( myptr, mysize );

   /* ... library code ... */

   MPI_Buffer_detach( &dummy, &idummy );
   MPI_Buffer_attach( ptr, size );

This is much like the action of the UNIX signal routine and has the same
strengths (it's simple) and weak‐nesses (it only works for nested
usages).

For Fortran: The Fortran binding for this routine is different. Because
Fortran does not have pointers, it is impossible to provide a way to use
the output of this routine to exchange buffers. In this case, only the
size field is set.

For C: Even though the buf argument is declared as void, it is really
the address of a void pointer. See Rationale, below, for more details.

Even though the C functions :ref:`MPI_Buffer_attach` and :ref:`MPI_Buffer_detach` both
have a first argument of type void*, these arguments are used
differently: A pointer to the buffer is passed to :ref:`MPI_Buffer_attach`; the
address of the pointer is passed to :ref:`MPI_Buffer_detach`, so that this call
can return the pointer value.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Buffer_attach`
