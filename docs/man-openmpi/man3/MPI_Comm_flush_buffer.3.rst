.. _mpi_comm_flush_buffer:

MPI_Comm_flush_buffer
======================

.. include_body

:ref:`MPI_Comm_flush_buffer`, :ref:`MPI_Comm_iflush_buffer` |mdash| Wait till all messages currently in 
the communicator-specific buffer of the calling MPI process have been transmitted.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Comm_flush_buffer, MPI_Comm_iflush_buffer

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_flush_buffer.rst

INPUT PARAMETERS
----------------

* ``comm``: Communicator (handle).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_flush_buffer`  will not return until all messages currently in the communicator-
specific buffer of the calling MPI process have been transmitted.  The non-blocking variant
:ref:`MPI_Comm_iflush_buffer` returns an MPI request that can be queried using MPI request
status functions to determine when all messages in the communicator-specific buffer of the
call MPI process have been transmitted.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_attach_buffer`
   * :ref:`MPI_Comm_detach_buffer`
