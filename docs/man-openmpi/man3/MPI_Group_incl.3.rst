.. _mpi_group_incl:

MPI_Group_incl
==============

.. include_body

:ref:`MPI_Group_incl` - Produces a group by reordering an existing group and
taking only listed members.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Group_incl(MPI_Group group, int n, const int ranks[],
       MPI_Group *newgroup)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GROUP_INCL(GROUP, N, RANKS, NEWGROUP, IERROR)
       INTEGER GROUP, N, RANKS(*), NEWGROUP, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Group_incl(group, n, ranks, newgroup, ierror)
       TYPE(MPI_Group), INTENT(IN) :: group
       INTEGER, INTENT(IN) :: n, ranks(n)
       TYPE(MPI_Group), INTENT(OUT) :: newgroup
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  group : Group (handle).
-  n : Number of elements in array ranks (and size of
   newgroup)(integer).
-  ranks : Ranks of processes in group to appear in newgroup (array of
   integers).

Output Parameters
-----------------

-  newgroup : New group derived from above, in the order defined by
   ranks (handle).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

The function :ref:`MPI_Group_incl` creates a group group_out that consists of
the n processes in group with ranks rank[0], ..., rank[n-1]; the process
with rank i in group_out is the process with rank ranks[i] in group.
Each of the n elements of ranks must be a valid rank in group and all
elements must be distinct, or else the program is erroneous. If n = 0,
then group_out is MPI_GROUP_EMPTY. This function can, for instance, be
used to reorder the elements of a group.

Note
----

This implementation does not currently check to ensure that there are no
duplicates in the list of ranks.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso:: :ref:`MPI_Group_compare`
