.. _mpi_session_attach_buffer:

MPI_Session_attach_buffer
=========================

.. include_body

:ref:`MPI_Session_attach_buffer` |mdash| Attaches a user-defined buffer to a session for sending.

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_attach_buffer.rst

INPUT PARAMETERS
----------------

* ``session``: Session (handle).
* ``buf`` : Initial buffer address (choice). 
* ``size`` : Buffer size, in bytes (integer). Ignored if buf is MPI_BUFFER_AUTOMATIC.

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Provides to MPI a buffer to attach to a session to be used for buffering
outgoing messages. The buffer is used only by messages sent in buffered
mode using the supplied session. Only one buffer can be attached to a session at a time.

NOTES
-----

This buffer is to be used for buffering outgoing messages sent when using a communicator 
that is created from a group that is derived from the session session. However, if there 
is a communicator-specific buffer attached to the particular communicator at the time 
of the buffered mode send is started, that buffer is used.

When not using MPI_BUFFER_AUTOMATIC, the size given should be the sum 
of the sizes of all outstanding Bsends that you intend to have, 
plus MPI_BSEND_OVERHEAD bytes for each Bsend that you do. For the purposes of 
calculating size, you should use :ref:`MPI_Pack_size`. In other words, in the code

.. code-block:: c

   MPI_Session_attach_buffer(session,  buf, size )
   MPI_Bsend( ..., count=20, datatype=type1, ... );
   MPI_Bsend( ..., count=40, datatype=type2, ... );

the value of size in the :ref:`MPI_Session_attach_buffer` call should be greater than
the value computed by

.. code-block:: c

   MPI_Pack_size( 20, type1, comm, &s1 );
   MPI_Pack_size( 40, type2, comm, &s2 );
   size = s1 + s2 + 2 * MPI_BSEND_OVERHEAD;

MPI_BSEND_OVERHEAD gives the maximum amount of buffer space that may be
used by the Bsend routines. This value is in mpi.h for C and mpif.h for
Fortran.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Session_detach_buffer`
   * :ref:`MPI_Comm_attach_buffer`
   * :ref:`MPI_Buffer_attach`
