.. _extended-callbacks-label:

Extended Callbacks (xcallbacks)
================================

This chapter documents the **Extended Callbacks** Open MPI extension,
which provides a set of ``MPIX_*_x`` C APIs that allow external language
bindings (particularly Fortran) to attach extra state and destructor
callbacks to MPI objects.

Quick Start
-----------

This is an extremely terse summary of how to use the extended callbacks
extension:

.. code-block:: c

  #include <mpi.h>
  #include <mpi-ext.h>

  void my_destructor(void *extra_state) {
      free(extra_state);
  }

  void my_user_function_x(void *invec, void *inoutvec, size_t count,
                          MPI_Datatype datatype, void *extra_state) {
      // Reduction operation using extra_state
  }

  int main(int argc, char **argv) {
      MPI_Init(&argc, &argv);

      void *state = malloc(sizeof(int));
      MPI_Op op;
      MPIX_Op_create_x(my_user_function_x, my_destructor,
                       1, state, &op);

      // Use op in reductions...
      MPI_Op_free(&op);  // Calls my_destructor(state)

      MPI_Finalize();
      return 0;
  }

Compile your application as usual, using the provided ``mpicc`` wrapper.

Features
--------

The Extended Callbacks extension provides a small set of C APIs that
extend the standard MPI object creation functions for operations,
error handlers, and attribute keyvals. These APIs are designed to
support external language bindings (particularly the MPICH standalone
Fortran bindings) by providing two key features:

1. **Extra state**: A ``void *extra_state`` pointer that is passed
   through to user callbacks, allowing external bindings to attach
   per-object state without relying on global variables or complex
   lookup tables.

2. **Destructor callbacks**: An ``MPIX_Destructor_function`` that is
   invoked when the owning MPI object is freed, enabling deterministic
   cleanup of resources associated with that object.

This extension implements the same ``MPIX_*_x`` C APIs provided by
MPICH, enabling source compatibility across both implementations for
applications and language bindings that use these features.

Available APIs
--------------

The extension provides the following C-only APIs:

**Operation creation:**

* :ref:`MPIX_Op_create_x` - Creates a user-defined reduction operation
  with extra state and a destructor callback. The user function
  receives a ``size_t`` count parameter (big count support only).

**Error handler creation:**

* :ref:`MPIX_Comm_create_errhandler_x` - Creates a communicator error
  handler with extra state and a destructor.
* :ref:`MPIX_Win_create_errhandler_x` - Creates a window error handler
  with extra state and a destructor.
* :ref:`MPIX_File_create_errhandler_x` - Creates a file error handler
  with extra state and a destructor.
* :ref:`MPIX_Session_create_errhandler_x` - Creates a session error
  handler with extra state and a destructor.

**Attribute keyval creation:**

* :ref:`MPIX_Comm_create_keyval_x` - Creates a communicator attribute
  keyval with copy/delete callbacks, extra state, and a destructor.
* :ref:`MPIX_Type_create_keyval_x` - Creates a datatype attribute
  keyval with copy/delete callbacks, extra state, and a destructor.
* :ref:`MPIX_Win_create_keyval_x` - Creates a window attribute keyval
  with copy/delete callbacks, extra state, and a destructor.

Extra State and Destructor Semantics
-------------------------------------

**Extra state pointer:**

The ``extra_state`` pointer passed to each ``MPIX_*_x`` creation
function is stored with the MPI object and passed to the appropriate
user callback whenever it is invoked:

* For operations, ``extra_state`` is passed to the
  ``MPIX_User_function_x`` callback on every reduction operation.
* For error handlers, ``extra_state`` is passed to the
  ``MPIX_*_errhandler_function_x`` callback whenever an error is raised.
* For attribute keyvals, ``extra_state`` is passed to the copy/delete
  callbacks whenever attributes are copied or deleted.

The ``extra_state`` pointer is not copied or managed by Open MPI. The
application (or language binding) is responsible for ensuring that it
remains valid for the lifetime of the object, until the destructor is
invoked.

**Destructor callback:**

The ``MPIX_Destructor_function`` passed to each ``MPIX_*_x`` creation
function has the signature:

.. code-block:: c

   typedef void (MPIX_Destructor_function)(void *extra_state);

This destructor is invoked exactly once when the MPI object is freed:

* For operations: when ``MPI_Op_free`` is called on a user-defined
  operation (never for predefined operations like ``MPI_SUM``).
* For error handlers: when the error handler's reference count drops to
  zero (typically when ``MPI_Errhandler_free`` is called).
* For attribute keyvals: when the keyval is freed with
  ``MPI_{Comm,Type,Win}_free_keyval``.

If the destructor function pointer is NULL, no destructor is invoked.

Building the Extension
----------------------

The Extended Callbacks extension is **built by default** in Open MPI
|ompi_ver|. It is automatically included when you build Open MPI unless
extensions are explicitly disabled.

To confirm that the extension is available in your installation:

.. code-block:: sh

   shell$ ompi_info | grep "MPI extensions"
          MPI extensions: affinity, cuda, ftmpi, rocm, xcallbacks

To explicitly control whether the extension is built, use the
``--enable-mpi-ext`` configure option:

.. code-block:: sh

   # Build all available extensions (default)
   shell$ ./configure --enable-mpi-ext

   # Build only specific extensions
   shell$ ./configure --enable-mpi-ext=xcallbacks,cuda

   # Disable all extensions
   shell$ ./configure --disable-mpi-ext

Using the Extension
-------------------

To use the Extended Callbacks APIs, include both ``<mpi.h>`` and
``<mpi-ext.h>`` in your C code:

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

The header ``<mpi-ext.h>`` defines the preprocessor macro
``OMPI_HAVE_MPI_EXT_XCALLBACKS`` to ``1`` when the extension is present,
allowing you to conditionally compile code that uses these APIs.

Compile your application using the standard Open MPI wrapper compilers:

.. code-block:: sh

   shell$ mpicc my_program.c -o my_program
   shell$ mpirun -n 4 ./my_program

No special compile or link flags are needed beyond what the wrappers
provide automatically.

Compatibility with MPICH
-------------------------

The Extended Callbacks APIs in Open MPI are designed to be compatible
with the equivalent ``MPIX_*_x`` APIs in MPICH. The function signatures,
semantics, and callback types are identical, enabling portable use
across both implementations.

This cross-implementation compatibility is the primary motivation for
this extension: it allows external language bindings (such as the MPICH
standalone Fortran bindings) to work with either MPI implementation
without modification.

For reference, the MPICH documentation for these APIs is available at:

* `MPIX_Op_create_x <https://www.mpich.org/static/docs/v5.0.x/www3/MPIX_Op_create_x.html>`_
* `MPIX_Comm_create_errhandler_x <https://www.mpich.org/static/docs/v5.0.x/www3/MPIX_Comm_create_errhandler_x.html>`_
* `MPIX_Win_create_errhandler_x <https://www.mpich.org/static/docs/v5.0.x/www3/MPIX_Win_create_errhandler_x.html>`_
* `MPIX_File_create_errhandler_x <https://www.mpich.org/static/docs/v5.0.x/www3/MPIX_File_create_errhandler_x.html>`_
* `MPIX_Session_create_errhandler_x <https://www.mpich.org/static/docs/v5.0.x/www3/MPIX_Session_create_errhandler_x.html>`_

C-Only Bindings
---------------

This extension provides **C bindings only**. No Fortran bindings are
provided, as the extension exists specifically to support external
language bindings that implement their language-to-C layer.

Availability in MPI ABI Libraries
----------------------------------

The Extended Callbacks extension is available in **both** the Open MPI
ABI library (``libmpi``, used by applications compiled with ``mpicc``)
**and** the MPI Forum standard ABI library (``libmpi_abi``, used by
applications compiled with ``mpicc_abi``).

.. warning:: Applications built against the MPI Forum standard ABI
   (``libmpi_abi``) that use these ``MPIX_*`` symbols lose source
   portability to other MPI implementations that do not provide the
   Extended Callbacks extension.

This is an inherent characteristic of any ``MPIX_*`` extension: the MPI
Forum standard ABI is designed for portability across implementations,
but implementation-specific extensions are by definition not portable.
The decision to include this extension in ``libmpi_abi`` was made to
support external Fortran bindings that target the standard ABI, with the
understanding that such bindings are tied to implementations that provide
these specific ``MPIX_*`` APIs (currently Open MPI and MPICH).

Use Cases
---------

The primary use case for the Extended Callbacks extension is supporting
external language bindings, particularly Fortran. For example:

* The MPICH standalone Fortran bindings package requires these
  ``MPIX_*_x`` APIs in the underlying C MPI implementation.
* External bindings can use the ``extra_state`` pointer to store
  language-specific context (e.g., Fortran handle mappings, callback
  wrappers) without polluting the global namespace or maintaining
  complex lookup tables.
* The destructor callbacks enable deterministic cleanup when MPI objects
  are freed, preventing resource leaks in the language binding layer.

While these APIs can be used directly by C applications, their design is
specifically tailored for the needs of external language bindings.

Notes
-----

* All APIs in this extension are **C-only**. No Fortran bindings are
  provided.
* The extension is **enabled by default** and included in both
  ``libmpi`` and ``libmpi_abi``.
* The ``extra_state`` pointer is not copied or managed by Open MPI; the
  caller is responsible for its lifetime.
* Destructor callbacks are invoked exactly once when the MPI object is
  freed, and only for user-defined objects (never for predefined objects).
* The extension provides the same API surface as MPICH for
  cross-implementation portability.

See Also
--------

* :ref:`MPIX_Op_create_x`
* :ref:`MPIX_Comm_create_errhandler_x`
* :ref:`MPIX_Win_create_errhandler_x`
* :ref:`MPIX_File_create_errhandler_x`
* :ref:`MPIX_Session_create_errhandler_x`
* :ref:`MPIX_Comm_create_keyval_x`
* :ref:`MPIX_Type_create_keyval_x`
* :ref:`MPIX_Win_create_keyval_x`
* :ref:`ompi-features-extensions-label`
