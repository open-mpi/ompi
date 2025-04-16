.. _mpi_type_create_keyval:


MPI_Type_create_keyval
======================

.. include_body

:ref:`MPI_Type_create_keyval` |mdash| Generates a new attribute key for caching
on data types.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_create_keyval.rst

INPUT PARAMETERS
----------------
* ``type_copy_attr_fn``: Copy callback function for *type_keyval* (function).
* ``type_delete_attr_fn``: Delete callback function for *type_keyval* (function).
* ``extra_state``: Extra state for callback functions.

OUTPUT PARAMETERS
-----------------
* ``type_keyval``: Key value for future access (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_keyval` generates a new attribute key for caching on data
types. This routine partially replaces :ref:`MPI_Keyval_create`.

The argument *type_copy_attr_fn* may be specified as
MPI_TYPE_NULL_COPY_FN or MPI_TYPE_DUP_FN from C or Fortran.
MPI_TYPE_NULL_COPY_FN is a function that does nothing other than
returning *flag* = 0 and MPI_SUCCESS. MPI_TYPE_DUP_FN is a simple-minded
copy function that sets *flag* = 1, returns the value of
*attribute_val_in* in *attribute_val_out*, and returns MPI_SUCCESS.

The argument *type_delete_attr_fn* may be specified as
MPI_TYPE_NULL_DELETE_FN from C or Fortran. MPI_TYPE_NULL_DELETE_FN is a
function that does nothing beyond returning MPI_SUCCESS. The C callback
functions are:

.. code-block:: c

   typedef int MPI_Type_copy_attr_function(MPI_Datatype oldtype,
               int type_keyval, void *extra_state, void *attribute_val_in,
               void *attribute_val_out, int *flag);

   typedef int MPI_Type_delete_attr_function(MPI_Datatype type, int type_keyval,
                void *attribute_val, void *extra_state);

The Fortran callback functions are:

.. code-block:: fortran

   SUBROUTINE TYPE_COPY_ATTR_FN(OLDTYPE, TYPE_KEYVAL, EXTRA_STATE,
                ATTRIBUTE_VAL_IN, ATTRIBUTE_VAL_OUT, FLAG, IERROR)
       INTEGER OLDTYPE, TYPE KEYVAL, IERROR
       INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE,
           ATTRIBUTE_VAL_IN, ATTRIBUTE_VAL_OUT
       LOGICAL FLAG

   SUBROUTINE TYPE_DELETE_ATTR_FN(TYPE, TYPE_KEYVAL, ATTRIBUTE_VAL, EXTRA_STATE,
                IERROR)
       INTEGER TYPE, TYPE_KEYVAL, IERROR
       INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE VAL, EXTRA_STATE


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_free_keyval`
