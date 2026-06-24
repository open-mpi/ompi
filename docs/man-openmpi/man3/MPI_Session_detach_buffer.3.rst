.. _mpi_session_detach_buffer:

MPI_Session_detach_buffer
=========================

.. include_body

:ref:`MPI_Session_detach_buffer` |mdash| Removes an existing buffer previously attached to the session(for use in in :ref:`MPI_Bsend`,
etc.)

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_detach_buffer.rst

OUTPUT PARAMETERS
-----------------

* ``session``: Session (handle).
* ``buf`` : Initial buffer address (choice).
* ``size`` : Buffer size, in bytes (integer).  Undefined if MPI_BUFFER_AUTOMATIC was used when attaching.
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Detach the buffer currently associated with the session. The call returns the
address and the size of the detached buffer. This operation will block
until all messages currently in the buffer have been transmitted. Upon
return of this function, the user may reuse or deallocate the space
taken by the buffer.

Example: Calls to attach and detach buffers.

.. code-block:: c

   #define BUFFSIZE 10000

   int size char *buff;
   MPI_Session_attach_buffer(session, malloc(BUFFSIZE), BUFFSIZE);

   // a buffer of 10000 bytes can now be used by MPI_Bsend

   MPI_Session_detach_buffer(session,  &buff, &size); // Buffer size reduced to zero
   MPI_Session_attach_buffer(session,  buff, size); // Buffer of 10000 bytes available again

NOTES
-----

The reason that :ref:`MPI_Session_detach_buffer` returns the address and size of the
buffer being detached is to allow nested libraries to replace and
restore the buffer. For example, consider

.. code-block:: c

   int size, mysize, idummy;
   void *ptr, *myptr, *dummy;
   MPI_Session_detach_buffer(session, &ptr, &size );
   MPI_Session_attach_buffer(session,  myptr, mysize );

   /* ... library code ... */

   MPI_Session_detach_buffer(session,  &dummy, &idummy );
   MPI_Session_attach_buffer(session,  ptr, size );

This is much like the action of the UNIX signal routine and has the same
strengths (it's simple) and weak‐nesses (it only works for nested
usages).

For Fortran: The Fortran binding for this routine is different. Because
Fortran does not have pointers, it is impossible to provide a way to use
the output of this routine to exchange buffers. In this case, only the
size field is set.  Note this does not apply when using the mpi_f08 bindings.

For C: Even though the buf argument is declared as void, it is really
the address of a void pointer. See Rationale, below, for more details.

Even though the C functions :ref:`MPI_Session_attach_buffer` and :ref:`MPI_Session_detach_buffer` both
have an argument of type void*, these arguments are used
differently: A pointer to the buffer is passed to :ref:`MPI_Session_attach_buffer`; the
address of the pointer is passed to :ref:`MPI_Session_detach_buffer`, so that this call
can return the pointer value.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Session_attach_buffer`
