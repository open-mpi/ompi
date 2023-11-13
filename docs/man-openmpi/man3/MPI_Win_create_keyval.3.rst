.. _mpi_win_create_keyval:


MPI_Win_create_keyval
=====================

.. include_body

:ref:`MPI_Win_create_keyval` |mdash| Creates a keyval for a window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn,
   	MPI_Win_delete_attr_function *win_delete_attr_fn,
   	int *win_keyval, void *extra_state)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_CREATE_KEYVAL(WIN_COPY_ATTR_FN, WIN_DELETE_ATTR_FN,
   	WIN_KEYVAL, EXTRA_STATE, IERROR)
   	EXTERNAL WIN_COPY_ATTR_FN, WIN_DELETE_ATTR_FN
   	INTEGER WIN_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval,
   		extra_state, ierror)
   	PROCEDURE(MPI_Win_copy_attr_function) :: win_copy_attr_fn
   	PROCEDURE(MPI_Win_delete_attr_function) :: win_delete_attr_fn
   	INTEGER, INTENT(OUT) :: win_keyval
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win_copy_attr_fn``: Copy callback function for *win_keyval* (function).
* ``win_delete_attr_fn``: Delete callback function for *win_keyval* (function).
* ``extra_state``: Extra state for callback functions.

OUTPUT PARAMETERS
-----------------
* ``win_keyval``: Key value for future access (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The argument *win_copy_attr_fn* may be specified as MPI_WIN_NULL_COPY_FN
or MPI_WIN_DUP_FN from either C or Fortran. MPI_WIN_NULL_COPY_FN is a
function that serves only to return *flag* = 0 and MPI_SUCCESS.
MPI_WIN_DUP_FN is a simple-minded copy function that sets *flag* = 1,
returns the value of *attribute_val_in* in *attribute_val_out*, and
returns MPI_SUCCESS.

The argument *win_delete_attr_fn* may be specified as
MPI_WIN_NULL_DELETE_FN from either C or Fortran. MPI_WIN_NULL_DELETE_FN
is a function that serves only to return MPI_SUCCESS.

The C callback functions are:

.. code-block:: c

   typedef int MPI_Win_copy_attr_function(MPI_Win oldwin, int win_keyval,
                void *extra_state, void *attribute_val_in,
                void *attribute_val_out, int *flag);

   typedef int MPI_Win_delete_attr_function(MPI_Win win, int win_keyval,
                void *attribute_val, void *extra_state);

The Fortran callback functions are:

.. code-block:: fortran

   SUBROUTINE WIN_COPY_ATTR_FN(OLDWIN, WIN_KEYVAL, EXTRA_STATE,
   	ATTRIBUTE_VAL_IN, ATTRIBUTE_VAL_OUT, FLAG, IERROR)
   	INTEGER OLDWIN, WIN_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) EXTRA_STATE, ATTRIBUTE_VAL_IN,
   		ATTRIBUTE_VAL_OUT
   	LOGICAL FLAG

   SUBROUTINE WIN_DELETE_ATTR_FN(WIN, WIN_KEYVAL, ATTRIBUTE_VAL,
   	EXTRA_STATE, IERROR)
   	INTEGER WIN, WIN_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL, EXTRA_STATE


ERRORS
------

.. include:: ./ERRORS.rst
