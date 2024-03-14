.. _mpi_errors:
.. _open-mpi-errors:

MPI Errors
==========

.. include_body


All MPI routines (except :ref:`MPI_Wtime` and :ref:`MPI_Wtick`) return an
error value; C routines as the value of the function and Fortran
routines in the last argument. Before the value is returned, the current
MPI error handler is called. By default, this error handler aborts the
MPI job. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.

For more information on Open MPI error codes, see ``mpi.h`` in the
``include`` directory.

Standard error return classes for Open MPI:

.. list-table::
   :header-rows: 1

   * - Error name
     - Value
     - Description

   * - MPI_SUCCESS
     - 0
     - Successful return code.

   * - MPI_ERR_BUFFER
     - 1
     - Invalid buffer pointer.

   * - MPI_ERR_COUNT
     - 2
     - Invalid count argument.

   * - MPI_ERR_TYPE
     - 3
     - Invalid datatype argument.

   * - MPI_ERR_TAG
     - 4
     - Invalid tag argument.

   * - MPI_ERR_COMM
     - 5
     - Invalid communicator.

   * - MPI_ERR_RANK
     - 6
     - Invalid rank.

   * - MPI_ERR_REQUEST
     - 7
     - Invalid MPI_Request handle.

   * - MPI_ERR_ROOT
     - 8
     - Invalid root.

   * - MPI_ERR_GROUP
     - 9
     - Null group passed to function.

   * - MPI_ERR_OP
     - 10
     - Invalid operation.

   * - MPI_ERR_TOPOLOGY
     - 11
     - Invalid topology.

   * - MPI_ERR_DIMS
     - 12
     - Illegal dimension argument.

   * - MPI_ERR_ARG
     - 13
     - Invalid argument.

   * - MPI_ERR_UNKNOWN
     - 14
     - Unknown error.

   * - MPI_ERR_TRUNCATE
     - 15
     - Message truncated on receive.

   * - MPI_ERR_OTHER
     - 16
     - Other error; use Error_string.

   * - MPI_ERR_INTERN
     - 17
     - Internal error code.

   * - MPI_ERR_IN_STATUS
     - 18
     - Look in status for error value.

   * - MPI_ERR_PENDING
     - 19
     - Pending request.

   * - MPI_ERR_ACCESS
     - 20
     - Permission denied.

   * - MPI_ERR_AMODE
     - 21
     - Unsupported amode passed to open.

   * - MPI_ERR_ASSERT
     - 22
     - Invalid assert.

   * - MPI_ERR_BAD_FILE
     - 23
     - Invalid file name (for example, path name too long).

   * - MPI_ERR_BASE
     - 24
     - Invalid base.

   * - MPI_ERR_CONVERSION
     - 25
     - An error occurred in a user-supplied data-conversion function.

   * - MPI_ERR_DISP
     - 26
     - Invalid displacement.

   * - MPI_ERR_DUP_DATAREP
     - 27
     - Conversion functions could not be registered because a data
       representation identifier that was already defined was passed
       to :ref:`MPI_Register_datarep`.

   * - MPI_ERR_FILE_EXISTS
     - 28
     - File exists.


   * - MPI_ERR_FILE_IN_USE
     - 29
     - File operation could not be completed, as the file is currently
       open by some process.

   * - MPI_ERR_FILE
     - 30
     - Invalid file handle.

   * - MPI_ERR_INFO_KEY
     - 31
     - Illegal info key.

   * - MPI_ERR_INFO_NOKEY
     - 32
     - No such key.

   * - MPI_ERR_INFO_VALUE
     - 33
     - Illegal info value.

   * - MPI_ERR_INFO
     - 34
     - Invalid info object.

   * - MPI_ERR_IO
     - 35
     - I/O error.

   * - MPI_ERR_KEYVAL
     - 36
     - Illegal key value.

   * - MPI_ERR_LOCKTYPE
     - 37
     - Invalid locktype.

   * - MPI_ERR_NAME
     - 38
     - Name not found.

   * - MPI_ERR_NO_MEM
     - 39
     - Memory exhausted.

   * - MPI_ERR_NOT_SAME
     - 40
     - Collective argument not identical on all processes, or
       collective routines called in a different order by different
       processes.

   * - MPI_ERR_NO_SPACE
     - 41
     - Not enough space.

   * - MPI_ERR_NO_SUCH_FILE
     - 42
     - File (or directory) does not exist.

   * - MPI_ERR_PORT
     - 43
     - Invalid port.

   * - MPI_ERR_PROC_ABORTED
     - 74
     - Operation failed because a remote peer has aborted.

   * - MPI_ERR_QUOTA
     - 44
     - Quota exceeded.

   * - MPI_ERR_READ_ONLY
     - 45
     - Read-only file system.

   * - MPI_ERR_RMA_CONFLICT
     - 46
     - Conflicting accesses to window.

   * - MPI_ERR_RMA_SYNC
     - 47
     - Erroneous RMA synchronization.

   * - MPI_ERR_SERVICE
     - 48
     - Invalid publish/unpublish.

   * - MPI_ERR_SIZE
     - 49
     - Invalid size.

   * - MPI_ERR_SPAWN
     - 50
     - Error spawning.

   * - MPI_ERR_UNSUPPORTED_DATAREP
     - 51
     - Unsupported datarep passed to :ref:`MPI_File_set_view`.

   * - MPI_ERR_UNSUPPORTED_OPERATION
     - 52
     - Unsupported operation, such as seeking on a file that supports
       only sequential access.

   * - MPI_ERR_WIN
     - 53
     - Invalid window.

   * - MPI_T_ERR_MEMORY
     - 54
     - Out of memory.

   * - MPI_T_ERR_NOT_INITIALIZED
     - 55
     - Interface not initialized.

   * - MPI_T_ERR_CANNOT_INIT
     - 56
     - Interface not in the state to be initialized.

   * - MPI_T_ERR_INVALID_INDEX
     - 57
     - The enumeration index is invalid.

   * - MPI_T_ERR_INVALID_ITEM
     - 8
     - The item index queried is out of range.

   * - MPI_T_ERR_INVALID_HANDLE
     - 59
     - The handle is invalid.

   * - MPI_T_ERR_OUT_OF_HANDLES
     - 60
     - No more handles available.

   * - MPI_T_ERR_OUT_OF_SESSIONS
     - 61
     - No more sessions available.

   * - MPI_T_ERR_INVALID_SESSION
     - 62
     - Session argument is not a valid session.

   * - MPI_T_ERR_CVAR_SET_NOT_NOW
     - 63
     - Variable cannot be set at this moment.

   * - MPI_T_ERR_CVAR_SET_NEVER
     - 64
     - Variable cannot be set until end of execution.

   * - MPI_T_ERR_PVAR_NO_STARTSTOP
     - 65
     - Variable cannot be started or stopped.

   * - MPI_T_ERR_PVAR_NO_WRITE
     - 6
     - Variable cannot be written or reset.

   * - MPI_T_ERR_PVAR_NO_ATOMIC
     - 67
     - Variable cannot be read and written atomically.

   * - MPI_ERR_RMA_RANGE
     - 68
     - Target memory is not part of the window (in the case of a
       window created with :ref:`MPI_Win_create_dynamic`, target memory is not attached.

   * - MPI_ERR_RMA_ATTACH
     - 69
     - Memory cannot be attached (e.g., because of resource
       exhaustion).

   * - MPI_ERR_RMA_FLAVOR
     - 70
     - Passed window has the wrong flavor for the called function.

   * - MPI_ERR_RMA_SHARED
     - 71
     - Memory cannot be shared (e.g., some process in the group of the
       specified communicator cannot expose shared memory).

   * - MPI_T_ERR_INVALID
     - 72
     - Invalid use of the interface or bad parameter values(s).

   * - MPI_T_ERR_INVALID_NAME
     - 73
     - The variable or category name is invalid.

   * - MPI_ERR_SESSION
     - 78
     - Invalid session

   * - MPI_ERR_LASTCODE
     - 93
     - Last error code.

.. seealso:: :ref:`MPI_T` :ref:`mpirun(1) <man1-mpirun>` :ref:`mpiexec(1)
             <man1-mpiexec>` :ref:`ompi_info(1) <man1-ompi_info>`
