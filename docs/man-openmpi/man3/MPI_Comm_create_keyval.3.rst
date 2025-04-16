.. _mpi_comm_create_keyval:

MPI_Comm_create_keyval
======================

.. include_body

:ref:`MPI_Comm_create_keyval` |mdash| Generates a new attribute key.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_create_keyval.rst

INPUT PARAMETERS
----------------

* ``comm_copy_attr_fn`` : Copy callback function for ``comm_keyval``
   (function).
* ``comm_delete_attr_fn`` : Delete callback function for
   ``comm_keyval`` (function).
* ``extra_state`` : Extra state for callback functions.

OUTPUT PARAMETER
----------------

* ``comm_keyval`` : Key value for future access (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This function replaces :ref:`MPI_Keyval_create`, the use of which is
deprecated. The C binding is identical. The Fortran binding differs in
that ``extra_state`` is an address-sized integer. Also, the copy and
delete callback functions have Fortran bindings that are consistent with
address-sized attributes. The argument ``comm_copy_attr_fn`` may be
specified as ``MPI_COMM_NULL_COPY_FN`` or ``MPI_COMM_DUP_FN`` from C or
Fortran. ``MPI_COMM_NULL_COPY_FN`` is a function that does nothing more
than returning ``flag = 0`` and ``MPI_SUCCESS``. ``MPI_COMM_DUP_FN`` is
a simple-minded copy function that sets ``flag = 1``, returns the value
of ``attribute_val_in`` in ``attribute_val_out``, and returns
``MPI_SUCCESS``. These replace the MPI-1 predefined callbacks
``MPI_NULL_COPY_FN`` and ``MPI_DUP_FN``, the use of which is deprecated.
The two C callback functions are:

.. code-block:: c

   typedef int MPI_Comm_copy_attr_function(MPI_Comm oldcomm, int comm_keyval,
                void *extra_state, void *attribute_val_in,
                void *attribute_val_out, int *flag);

   typedef int MPI_Comm_delete_attr_function(MPI_Comm comm, int comm_keyval,
                void *attribute_val, void *extra_state);

which are the same as the MPI-1.1 calls but with a new name. The old
names are deprecated. The two Fortran callback functions are:

.. code-block:: fortran

   SUBROUTINE COMM_COPY_ATTR_FN(OLDCOMM, COMM_KEYVAL, EXTRA_STATE,
                ATTRIBUTE_VAL_IN, ATTRIBUTE_VAL_OUT, FLAG, IERROR)
       INTEGER OLDCOMM, COMM_KEYVAL, IERROR

       INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE, ATTRIBUTE_VAL_IN,
           ATTRIBUTE_VAL_OUT
       LOGICAL FLAG

   SUBROUTINE COMM_DELETE_ATTR_FN(COMM, COMM_KEYVAL, ATTRIBUTE_VAL, EXTRA_STATE,
                IERROR)
       INTEGER COMM, COMM_KEYVAL, IERROR

       INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL, EXTRA_STATE


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_free_keyval`
