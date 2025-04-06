.. _mpi_comm_call_errhandler:

MPI_Comm_call_errhandler
========================

.. include_body

:ref:`MPI_Comm_call_errhandler` |mdash| Passes the supplied error code to the error
handler assigned to a communicator

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_call_errhandler.rst

INPUT PARAMETER
---------------

* ``comm`` : communicator with error handler (handle).
* ``errorcode`` : error code (integer).

OUTPUT PARAMETERS
-----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This function invokes the error handler assigned to the communicator
comm with the supplied error code errorcode. If the error handler was
successfully called, the process is not aborted, and the error handler
returns, this function returns MPI_SUCCESS.

NOTES
-----

Users should note that the default error handler is
MPI_ERRORS_ARE_FATAL. Thus, calling this function will abort the
processes in comm if the default error handler has not been changed.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_errhandler`
