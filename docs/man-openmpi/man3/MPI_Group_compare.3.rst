.. _mpi_group_compare:

MPI_Group_compare
=================

.. include_body

:ref:`MPI_Group_compare` - Compares two groups.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Group_compare(MPI_Group group1, MPI_Group group2,
       int *result)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GROUP_COMPARE(GROUP1, GROUP2, RESULT, IERROR)
       INTEGER GROUP1, GROUP2, RESULT, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Group_compare(group1, group2, result, ierror)
       TYPE(MPI_Group), INTENT(IN) :: group1, group2
       INTEGER, INTENT(OUT) :: result
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  ``group1`` : First group (handle).
-  ``group2`` : Second group (handle).

Output Parameters
-----------------

-  ``result`` : Integer which is MPI_IDENT if the order and members of
   the two groups are the same, MPI_SIMILAR if only the members are the
   same, and MPI_UNEQUAL otherwise.
-  ``IERROR`` : Fortran only: Error status (integer).

Description
-----------

MPI_IDENT results if the group members and group order is exactly
the same in both groups. This happens for instance if ``group1`` and
``group2`` are the same handle. MPI_SIMILAR results if the group
members are the same but the order is different. MPI_UNEQUAL results
otherwise.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler
MPI_ERRORS_RETURN may be used to cause error values to be returned.
Note that MPI does not guarantee that an MPI program can continue past
an error.
