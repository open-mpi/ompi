.. _mpi_buffer_flush:

MPI_Buffer_flush
================

.. include_body

:ref:`MPI_Buffer_flush`, :ref:`MPI_Buffer_iflush` |mdash| Wait till all messages currently in 
the the MPI process specific buffer of the calling MPI process have been transmitted.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Buffer_flush, MPI_Buffer_iflush

.. The following file was automatically generated
.. include:: ./bindings/mpi_buffer_flush.rst

INPUT PARAMETERS
----------------

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Buffer_flush`  will not return until all messages currently in the MPI process
specific buffer of the calling MPI process have been transmitted.  The non-blocking variant
:ref:`MPI_Buffer_iflush` returns a MPI request that can be queried using MPI request
status functions to determine when all messages in the MPI process specific  buffer of the
call MPI process have been transmitted.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Buffer_attach`
   * :ref:`MPI_Buffer_detach`
