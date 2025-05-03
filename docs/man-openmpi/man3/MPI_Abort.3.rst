.. _mpi_abort:


MPI_Abort
=========

.. include_body

:ref:`MPI_Abort` |mdash| Terminates MPI execution environment.

.. The following file was automatically generated
.. include:: ./bindings/mpi_abort.rst

INPUT PARAMETERS
----------------
* ``comm``: Communicator of tasks to abort.
* ``errorcode``: Error code to return to invoking environment.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine makes a "best attempt" to abort all tasks in the group of
comm. This function does not require that the invoking environment take
any action with the error code. However, a UNIX or POSIX environment
should handle this as a return errorcode from the main program or an
abort (errorcode).

The long-term goal of the Open MPI implementation is to terminate all
processes in all tasks that contain a process in *comm, and the error
code is not returned to the invoking environment. At the moment, this
isn't fully implemented and* :ref:`MPI_Abort` *will terminate the entire job.*

Note: All associated processes are sent a ``SIGTERM``.


ERRORS
------
.. include:: ./ERRORS.rst
