.. _mpi_comm_free_keyval:


MPI_Comm_free_keyval
====================

.. include_body

:ref:`MPI_Comm_free_keyval` - Frees attribute key for communicator cache
attribute.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_free_keyval(int *comm_keyval)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_FREE_KEYVAL(COMM_KEYVAL, IERROR)
   	INTEGER	COMM_KEYVAL, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_free_keyval(comm_keyval, ierror)
   	INTEGER, INTENT(INOUT) :: comm_keyval
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``comm_keyval``:

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_free_keyval` frees an extant attribute key. This function sets
the value of *keyval* to MPI_KEYVAL_INVALID. Note that it is not
erroneous to free an attribute key that is in use, because the actual
free does not transpire until after all references (in other
communicators on the process) to the key have been freed. These
references need to be explicitly freed by the program, either via calls
to :ref:`MPI_Comm_delete_attr` that free one attribute instance, or by calls to
:ref:`MPI_Comm_free` that free all attribute instances associated with the
freed communicator.

This call is identical to the call :ref:`MPI_Keyval_free` but is needed to
match the communicator-specific creation function introduced in the
MPI-2 standard. The use of :ref:`MPI_Keyval_free` is deprecated.


NOTES
-----

Key values are global (they can be used with any and all communicators).


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
