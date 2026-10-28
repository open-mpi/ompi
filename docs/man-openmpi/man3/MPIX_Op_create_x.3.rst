.. _mpix_op_create_x:

MPIX_Op_create_x
================

.. include_body

:ref:`MPIX_Op_create_x` - Creates a user-defined reduction operation
with extra state and a destructor callback.

This is part of the Extended Callbacks MPI extension (xcallbacks).

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Op_create_x(MPIX_User_function_x *user_fn_x,
                        MPIX_Destructor_function *destructor_fn,
                        int commute,
                        void *extra_state,
                        MPI_Op *op)

INPUT PARAMETERS
----------------

* ``user_fn_x``: User-defined reduction function (function pointer).
* ``destructor_fn``: User-defined destructor function invoked when *op* is freed (function pointer, may be NULL).
* ``commute``: True if the reduction operation is commutative; false otherwise (logical).
* ``extra_state``: Pointer to extra state passed to *user_fn_x* (may be NULL).

OUTPUT PARAMETERS
-----------------

* ``op``: Reduction operation handle (handle).

DESCRIPTION
-----------

:ref:`MPIX_Op_create_x` is an extended variant of :ref:`MPI_Op_create`
that creates a user-defined reduction operation with two additional
features:

1. **Extra state**: The *extra_state* pointer is passed through to the
   user reduction function on every invocation, allowing the function to
   access per-operation context without relying on global variables.

2. **Destructor callback**: When the operation is freed with
   :ref:`MPI_Op_free`, Open MPI invokes *destructor_fn* (if non-NULL)
   with *extra_state* as the argument, allowing deterministic cleanup of
   resources associated with the operation.

The user-defined reduction function *user_fn_x* has the signature:

.. code-block:: c

   typedef void (MPIX_User_function_x)(void *invec,
                                        void *inoutvec,
                                        size_t count,
                                        MPI_Datatype datatype,
                                        void *extra_state);

This differs from the standard ``MPI_User_function`` in two ways:

* The *count* and *datatype* parameters are passed by value (not by
  address).
* An additional *extra_state* parameter is provided.

The *count* parameter uses ``size_t`` to support large counts (big
count). This is the only count representation supported by
:ref:`MPIX_Op_create_x`; there is no small-count variant.

The destructor function *destructor_fn* has the signature:

.. code-block:: c

   typedef void (MPIX_Destructor_function)(void *extra_state);

The destructor is invoked exactly once when the operation is freed with
:ref:`MPI_Op_free`, and only for user-defined operations (never for
predefined operations like ``MPI_SUM``). If *destructor_fn* is NULL, no
destructor is invoked.

The operation *op* can be used in any MPI reduction operation (e.g.,
:ref:`MPI_Reduce`, :ref:`MPI_Allreduce`, :ref:`MPI_Reduce_scatter`,
:ref:`MPI_Scan`) just like operations created with :ref:`MPI_Op_create`.

NOTES
-----

* The *user_fn_x* function must be associative: ``(a op b) op c == a op (b op c)``.
  If *commute* is true, it must also be commutative: ``a op b == b op a``.

* The *extra_state* pointer is not copied or managed by Open MPI; the
  user is responsible for ensuring it remains valid for the lifetime of
  the operation (until *destructor_fn* is invoked).

* This function is C-only. No Fortran bindings are provided.

* This API is compatible with MPICH's ``MPIX_Op_create_x``, enabling
  portable use across both implementations.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Op_create`
   * :ref:`MPI_Op_free`
   * :ref:`MPI_Reduce`
   * :ref:`MPI_Allreduce`
