.. _mpi_session_flush_buffer:

MPI_Session_flush_buffer
========================

.. include_body

:ref:`MPI_Session_flush_buffer`, :ref:`MPI_Session_iflush_buffer` |mdash| Wait till all messages currently in the session-specific 
buffer of the calling MPI process have been transmitted.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Session_flush_buffer, MPI_Session_iflush_buffer

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_flush_buffer.rst

INPUT PARAMETERS
----------------

* ``session``: Session (handle).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_flush_buffer`  will not return until all messages currently in the session-specific 
buffer of the calling MPI process have been transmitted.  The non-blocking variant
:ref:`MPI_Session_iflush_buffer` returns a MPI request that can be queried using MPI request
status functions to determine when all messages in the sessionunicator-specific buffer of the
call MPI process have been transmitted.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Session_attach_buffer`
   * :ref:`MPI_Session_detach_buffer`
