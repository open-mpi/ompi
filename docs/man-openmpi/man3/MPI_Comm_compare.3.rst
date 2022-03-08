.. _mpi_comm_compare:

MPI_Comm_compare
================

.. include_body

:ref:`MPI_Comm_compare` - Compares two communicators.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_COMPARE(COMM1, COMM2, RESULT, IERROR)
       INTEGER COMM1, COMM2, RESULT, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Comm_compare(comm1, comm2, result, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm1, comm2
       INTEGER, INTENT(OUT) :: result
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  ``comm1`` : Comm1 (handle).
-  ``comm2`` : Comm2 (handle).

Output Parameters
-----------------

-  ``result`` : Result of comparison (integer).
-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

MPI_IDENT ``result``\ s if and only if ``comm1`` and ``comm2`` are
handles for the same object (identical groups and same contexts).
MPI_CONGRUENT results if the underlying groups are identical in
constituents and rank order; these communicators differ only by context.
MPI_SIMILAR results of the group members of both communicators are
the same but the rank order differs. MPI_UNEQUAL results otherwise.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
