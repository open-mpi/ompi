.. _mpi_group_translate_ranks:


MPI_Group_translate_ranks
=========================

.. include_body

:ref:`MPI_Group_translate_ranks` - Translates the ranks of processes in one
group to those in another group.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_translate_ranks(MPI_Group group1, int n,
   	const int ranks1[], MPI_Group group2, int ranks2[])


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_GROUP_TRANSLATE_RANKS(GROUP1, N, RANKS1, GROUP2, RANKS2,
   		IERROR)
   	INTEGER	GROUP1, N, RANKS1(*), GROUP2, RANKS2(*), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2, ierror)
   	TYPE(MPI_Group), INTENT(IN) :: group1, group2
   	INTEGER, INTENT(IN) :: n, ranks1(n)
   	INTEGER, INTENT(OUT) :: ranks2(n)
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``group1``: First group (handle).
* ``n``: Number of ranks in ranks1 and ranks2 arrays (integer).
* ``ranks1``: Array of zero or more valid ranks in group1.
* ``group2``: Second group (handle).

OUTPUT PARAMETERS
-----------------
* ``ranks2``: Array of corresponding ranks in group2, MPI_UNDEFINED when no correspondence exists.
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function is important for determining the relative numbering of the
same processes in two different groups. For instance, if one knows the
ranks of certain processes in the group of MPI_COMM_WORLD, one might
want to know their ranks in a subset of that group.


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
