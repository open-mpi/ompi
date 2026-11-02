.. _mpix_session_create_errhandler_x:

MPIX_Session_create_errhandler_x
=================================

.. include_body

NAME
----

**MPIX_Session_create_errhandler_x** - Creates an error handler with
extended callback support for sessions.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Session_create_errhandler_x(MPIX_Session_errhandler_function_x *session_errhandler_fn_x,
                                         MPIX_Destructor_function *destructor_fn,
                                         void *extra_state,
                                         MPI_Errhandler *errhandler)

INPUT PARAMETERS
----------------

* ``session_errhandler_fn_x`` : User-defined error handling procedure with extra_state support (function).
* ``destructor_fn`` : User-defined destructor invoked when the error handler is freed (function).
* ``extra_state`` : Extra state passed to the error handler and destructor (choice).

OUTPUT PARAMETERS
-----------------

* ``errhandler`` : MPI error handler (handle).

CALLBACK SIGNATURES
-------------------

.. code-block:: c

   typedef void MPIX_Session_errhandler_function_x(MPI_Session session, int error_code,
                                                    void *extra_state);

   typedef void MPIX_Destructor_function(void *extra_state);

DESCRIPTION
-----------

:ref:`MPIX_Session_create_errhandler_x` creates an error handler that can be
attached to sessions. This is an extended version of
``MPI_Session_create_errhandler`` that accepts an ``extra_state`` parameter
passed to the error handler callback, plus a destructor function invoked
when the error handler is freed.

The user-defined error handling procedure
``session_errhandler_fn_x`` is called with the session (by value), error
code, and the ``extra_state`` pointer provided at creation time. Unlike
the standard ``MPI_Session_errhandler_function``, the extended callback
receives the session by value (not as a pointer) and does not receive a
message string.

The ``destructor_fn`` is invoked exactly once when the error handler's
reference count drops to zero (typically during ``MPI_Errhandler_free``
or when the last object using it is destroyed). The destructor receives
the same ``extra_state`` pointer, allowing the application to clean up
resources associated with the error handler.

This API is compatible with MPICH's standalone Fortran bindings and is
available in both the Open MPI ABI (``libmpi``) and the MPI Forum
standard ABI (``libmpi_abi``) when Open MPI is configured with
``--enable-mpi-ext=xcallbacks``.

NOTES
-----

This function is part of the ``xcallbacks`` MPI extension. It is only
available when Open MPI is built with ``--enable-mpi-ext=xcallbacks``
(enabled by default). Programs using this function must include
``<mpi-ext.h>`` in addition to ``<mpi.h>``.

The ``extra_state`` pointer is stored in the error handler and passed
unmodified to both the error handler callback and the destructor. The
application is responsible for ensuring the pointer remains valid for the
lifetime of the error handler.

Unlike standard MPI error handlers, the extended callback signature
passes the session by value. This is a C-only API; no Fortran bindings
are provided.

ERRORS
------

Almost all MPI routines return an error value; C routines as the return result
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler associated
with the communication object (e.g., communicator, window, file) is called.
If no communication object is associated with the MPI call, then the call is
considered attached to MPI_COMM_SELF and will call the associated MPI error
handler. When MPI_COMM_SELF is not initialized (i.e., before
:ref:`MPI_Init`/:ref:`MPI_Init_thread`, after :ref:`MPI_Finalize`, or when using the Sessions
Model exclusively) the error raises the initial MPI error handler. The initial
MPI error handler can be changed by calling :ref:`MPI_Comm_set_errhandler` on
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
   window, file, or session. When invoked on a communicator, it
   acts as if :ref:`MPI_Abort` was called on that communicator. If
   invoked on a window or file, acts as if :ref:`MPI_Abort` was called
   on a communicator containing the group of processes in the
   corresponding window or file. If invoked on a session,
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

See the Error Handling section of the MPI-3.1 standard for
more information.


.. seealso::
   * :ref:`MPI_Session_create_errhandler`
   * :ref:`MPI_Session_set_errhandler`
   * :ref:`MPI_Session_get_errhandler`
   * :ref:`MPI_Errhandler_free`
   * :ref:`MPIX_Comm_create_errhandler_x`
   * :ref:`MPIX_Win_create_errhandler_x`
   * :ref:`MPIX_File_create_errhandler_x`
