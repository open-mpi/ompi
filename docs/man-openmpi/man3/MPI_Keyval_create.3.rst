.. _mpi_keyval_create:


MPI_Keyval_create
=================

.. include_body

:ref:`MPI_Keyval_create` |mdash| Generates a new attribute key |mdash| |deprecated_favor| :ref:`MPI_Comm_create_keyval`.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Keyval_create(MPI_Copy_function *copy_fn,
   	MPI_Delete_function *delete_fn, int *keyval, void *extra_state)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_KEYVAL_CREATE(COPY_FN, DELETE_FN, KEYVAL, EXTRA_STATE, IERROR)
   	EXTERNAL	COPY_FN, DELETE_FN
   	INTEGER	KEYVAL, EXTRA_STATE, IERROR


INPUT PARAMETERS
----------------
* ``copy_fn``: Copy callback function for keyval.
* ``delete_fn``: Delete callback function for keyval.
* ``extra_state``: Extra state for callback functions.

OUTPUT PARAMETERS
-----------------
* ``keyval``: Key value for future access (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Please use
:ref:`MPI_Comm_create_keyval` instead.

Generates a new attribute key. Keys are locally unique in a process and
opaque to the user, though they are explicitly stored in integers. Once
allocated, the key value can be used to associate attributes and access
them on any locally defined communicator.

The copy_fn function is invoked when a communicator is duplicated by
:ref:`MPI_Comm_dup`. copy_fn should be of type MPI_Copy_function, which is
defined as follows:

.. code-block:: c

     typedef int MPI_Copy_function(MPI_Comm oldcomm, int keyval,
                                   void *extra_state, void *attribute_val_in,
                                   void *attribute_val_out, int *flag)

A Fortran declaration for such a function is as follows:

.. code-block:: fortran

     SUBROUTINE COPY_FUNCTION(OLDCOMM, KEYVAL, EXTRA_STATE, ATTRIBUTE_VAL_IN,
                 ATTRIBUTE_VAL_OUT, FLAG, IERR)
     INTEGER OLDCOMM, KEYVAL, EXTRA_STATE,
     ATTRIBUTE_VAL_IN, ATTRIBUTE_VAL_OUT, IERR
     LOGICAL FLAG

The copy callback function is invoked for each key value in oldcomm in
arbitrary order. Each call to the copy callback is made with a key value
and its corresponding attribute. If it returns flag = 0, then the
attribute is deleted in the duplicated communicator. Otherwise ( flag =
1), the new attribute value is set to the value returned in
attribute_val_out. The function returns MPI_SUCCESS on success and an
error code on failure (in which case :ref:`MPI_Comm_dup` will fail).

copy_fn may be specified as MPI_NULL_COPY_FN or MPI_DUP_FN from either C
or Fortran; MPI_NULL_COPY_FN is a function that does nothing other than
return flag = 0, and MPI_SUCCESS. MPI_DUP_FN is a simple-minded copy
function that sets flag = 1, returns the value of attribute_val_in in
attribute_val_out, and returns MPI_SUCCESS.


NOTES
-----

Key values are global (available for any and all communicators).

There are subtle differences between C and Fortran that require that the
copy_fn be written in the same language that :ref:`MPI_Keyval_create` is called
from. This should not be a problem for most users; only programmers
using both Fortran and C in the same program need to be sure that they
follow this rule.

Even though both formal arguments attribute_val_in and attribute_val_out
are of type void*, their usage differs. The C copy function is passed by
MPI in attribute_val_in the value of the attribute, and in
attribute_val_out the address of the attribute, so as to allow the
function to return the (new) attribute value. The use of type ``void *`` for
both is to avoid messy type casts.

A valid copy function is one that completely duplicates the information
by making a full duplicate copy of the data structures implied by an
attribute; another might just make another reference to that data
structure, while using a reference-count mechanism. Other types of
attributes might not copy at all (they might be specific to oldcomm
only).

Analogous to copy_fn is a callback deletion function, defined as
follows. The delete_fn function is invoked when a communicator is
deleted by :ref:`MPI_Comm_free` or when a call is made explicitly to
:ref:`MPI_Attr_delete`. delete_fn should be of type MPI_Delete_function, which
is defined as follows:

.. code-block:: c

     typedef int MPI_Delete_function(MPI_Comm comm, int keyval,
         void *attribute_val, void *extra_state);

A Fortran declaration for such a function is as follows:

.. code-block:: fortran

     SUBROUTINE DELETE_FUNCTION(COMM, KEYVAL,ATTRIBUTE_VAL, EXTRA_STATE, IERR)
         INTEGER COMM, KEYVAL, ATTRIBUTE_VAL, EXTRA_STATE, IERR

This function is called by :ref:`MPI_Comm_free`, :ref:`MPI_Attr_delete`, and
:ref:`MPI_Attr_put` to do whatever is needed to remove an attribute. The
function returns MPI_SUCCESS on success and an error code on failure (in
which case :ref:`MPI_COMM_FREE` will fail).

delete_fn may be specified as MPI_NULL_DELETE_FN from either C or
FORTRAN; MPI_NULL_DELETE_FN is a function that does nothing, other than
returning MPI_SUCCESS.

The special key value MPI_KEYVAL_INVALID is never returned by
:ref:`MPI_Keyval_create`. Therefore, it can be used for static initialization
of key values.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Keyval_free`
   * :ref:`MPI_Comm_create_keyval`
   * :ref:`MPI_Comm_free_keyval`
