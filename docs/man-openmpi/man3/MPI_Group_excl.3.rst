.. _mpi_group_excl:

MPI_Group_excl
==============

.. include_body

:ref:`MPI_Group_excl` |mdash| Produces a group by reordering an existing group and
taking only unlisted members.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_excl(MPI_Group group, int n, const int ranks[],
       MPI_Group *newgroup)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GROUP_EXCL(GROUP, N, RANKS, NEWGROUP, IERROR)
       INTEGER GROUP, N, RANKS(*), NEWGROUP, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Group_excl(group, n, ranks, newgroup, ierror)
       TYPE(MPI_Group), INTENT(IN) :: group
       INTEGER, INTENT(IN) :: n, ranks(n)
       TYPE(MPI_Group), INTENT(OUT) :: newgroup
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``group`` : Group (handle).
* ``n`` : Number of elements in array ranks (integer).
* ``ranks`` : Array of integer ranks in group not to appear in newgroup.

OUTPUT PARAMETERS
-----------------

* ``newgroup`` : New group derived from above, preserving the order defined
   by group (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Group_excl` creates a group of processes newgroup that
is obtained by deleting from group those processes with ranks ranks[0],
... ranks[n-1]. The ordering of processes in newgroup is identical to
the ordering in group. Each of the n elements of ranks must be a valid
rank in group and all elements must be distinct; otherwise, the call is
erroneous. If n = 0, then newgroup is identical to group.

NOTE
----

Currently, each of the ranks to exclude must be a valid rank in the
group and all elements must be distinct or the function is erroneous.
This restriction is per the draft.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Group_range_excl`
