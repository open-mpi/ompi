.. _mpi_win_get_attr:


MPI_Win_get_attr
================

.. include_body

:ref:`MPI_Win_get_attr` - Obtains the value of a window attribute.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_get_attr(MPI_Win win, int win_keyval,
   	void *attribute_val, int *flag)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_GET_ATTR(WIN, WIN_KEYVAL, ATTRIBUTE_VAL, FLAG, IERROR)
   	INTEGER WIN, WIN_KEYVAL, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ATTRIBUTE_VAL
   	LOGICAL FLAG


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_get_attr(win, win_keyval, attribute_val, flag, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, INTENT(IN) :: win_keyval
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   	LOGICAL, INTENT(OUT) :: flag
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win``: Window to which the attribute is attached (handle).
* ``win_keyval``: Key value (integer).

OUTPUT PARAMETERS
-----------------
* ``attribute_val``: Attribute value, unless *ag* = false
* ``flag``: False if no attribute is associated with the key (logical).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Obtains the value of a window attribute.


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the
*ATTRIBUTE_VAL* argument only for Fortran 90. FORTRAN 77 users may use
the non-portable syntax

::

        INTEGER*MPI_ADDRESS_KIND ATTRIBUTE_VAL

where MPI_ADDRESS_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.
