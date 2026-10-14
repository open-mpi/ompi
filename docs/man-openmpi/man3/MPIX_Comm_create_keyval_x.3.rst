.. _mpix_comm_create_keyval_x:

MPIX_Comm_create_keyval_x
==========================

.. include_body

NAME
----

**MPIX_Comm_create_keyval_x** - Generates a new attribute key with
extended callback support for communicators.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Comm_create_keyval_x(MPI_Comm_copy_attr_function *comm_copy_attr_fn,
                                  MPI_Comm_delete_attr_function *comm_delete_attr_fn,
                                  int *comm_keyval,
                                  void *extra_state,
                                  MPIX_Destructor_function *destructor_fn)

INPUT PARAMETERS
----------------

* ``comm_copy_attr_fn`` : Copy callback function for ``comm_keyval`` (function).
* ``comm_delete_attr_fn`` : Delete callback function for ``comm_keyval`` (function).
* ``extra_state`` : Extra state passed to copy and delete callback functions.
* ``destructor_fn`` : User-defined destructor invoked when the keyval is freed (function).

OUTPUT PARAMETERS
-----------------

* ``comm_keyval`` : Key value for future access (integer).

CALLBACK SIGNATURES
-------------------

.. code-block:: c

   typedef int MPI_Comm_copy_attr_function(MPI_Comm oldcomm, int comm_keyval,
                                            void *extra_state,
                                            void *attribute_val_in,
                                            void *attribute_val_out,
                                            int *flag);

   typedef int MPI_Comm_delete_attr_function(MPI_Comm comm, int comm_keyval,
                                              void *attribute_val,
                                              void *extra_state);

   typedef void MPIX_Destructor_function(void *extra_state);

DESCRIPTION
-----------

:ref:`MPIX_Comm_create_keyval_x` creates a communicator keyval with
extended callback support. This is an extended version of
:ref:`MPI_Comm_create_keyval` that accepts a destructor function in
addition to the standard copy and delete callbacks.

The copy and delete callbacks follow the same semantics as
:ref:`MPI_Comm_create_keyval` and receive the ``extra_state`` pointer
through the standard MPI attribute callback mechanism. The
``comm_copy_attr_fn`` may be specified as ``MPI_COMM_NULL_COPY_FN`` or
``MPI_COMM_DUP_FN``. ``MPI_COMM_NULL_COPY_FN`` is a
function that does nothing more than returning ``flag = 0`` and
``MPI_SUCCESS``. ``MPI_COMM_DUP_FN`` is a simple-minded copy function
that sets ``flag = 1``, returns the value of ``attribute_val_in`` in
``attribute_val_out``, and returns ``MPI_SUCCESS``.

The ``destructor_fn`` is invoked exactly once when the keyval's
reference count drops to zero (typically during
``MPI_Comm_free_keyval`` or when the last communicator using it is
destroyed). The destructor receives the same ``extra_state`` pointer,
allowing the application to clean up resources associated with the
keyval.

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

The ``extra_state`` pointer is stored in the keyval and passed
unmodified to both the copy/delete callbacks and the destructor. The
application is responsible for ensuring the pointer remains valid for
the lifetime of the keyval.

The destructor fires when the keyval itself is freed, not when
individual attributes are copied or deleted. This is independent of the
copy and delete callbacks, which fire during attribute operations on
communicators.

This is a C-only API; no Fortran bindings are provided.

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
   * :ref:`MPI_Comm_create_keyval`
   * :ref:`MPI_Comm_free_keyval`
   * :ref:`MPI_Comm_set_attr`
   * :ref:`MPI_Comm_get_attr`
   * :ref:`MPI_Comm_delete_attr`
   * :ref:`MPIX_Type_create_keyval_x`
   * :ref:`MPIX_Win_create_keyval_x`
