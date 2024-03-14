Almost all MPI routines return an error value; C routines as the return result
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler associated
with the communication object (e.g., communicator, window, file) is called.
If no communication object is associated with the MPI call, then the call is
considered attached to MPI_COMM_SELF and will call the associated MPI error
handler. When MPI_COMM_SELF is not initialized (i.e., before
:ref:`MPI_Init`/:ref:`MPI_Init_thread`, after :ref:`MPI_Finalize`, or when using the Sessions
Model exclusively) the error raises the initial error handler. The initial
error handler can be changed by calling :ref:`MPI_Comm_set_errhandler` on
MPI_COMM_SELF when using the World model, or the mpi_initial_errhandler CLI
argument to mpiexec or info key to :ref:`MPI_Comm_spawn`/:ref:`MPI_Comm_spawn_multiple`.
If no other appropriate error handler has been set, then the MPI_ERRORS_RETURN
error handler is called for MPI I/O functions and the MPI_ERRORS_ABORT error
handler is called for all other MPI functions.

Open MPI includes three predefined error handlers that can be used:

* ``MPI_ERRORS_ARE_FATAL``
  Causes the program to abort all connected MPI processes.
* ``MPI_ERRORS_ABORT``
  An error handler that can be invoked on a communicator,
  window, file, or session. When called on a communicator, it
  acts as if :ref:`MPI_Abort` was called on that communicator. If
  called on a window or file, acts as if :ref:`MPI_Abort` was called
  on a communicator containing the group of processes in the
  corresponding window or file. If called on a session,
  aborts only the local process.
* ``MPI_ERRORS_RETURN``
  Returns an error code to the application.

MPI applications can also implement their own error handlers by calling:

* :ref:`MPI_Comm_create_errhandler` then :ref:`MPI_Comm_set_errhandler`
* :ref:`MPI_File_create_errhandler` then :ref:`MPI_File_set_errhandler`
* :ref:`MPI_Session_create_errhandler` then :ref:`MPI_Session_set_errhandler` or at :ref:`MPI_Session_init`
* :ref:`MPI_Win_create_errhandler` then :ref:`MPI_Win_set_errhandler`

Note that MPI does not guarantee that an MPI program can continue past
an error.

See the :ref:`MPI man page <open-mpi>` for a full list of :ref:`MPI error codes <open-mpi-errors>`.

See the Error Handling section of the MPI-|mpi_standard_version| standard for
more information.

