.. _mpi_group_excl:

MPI_Group_excl
==============

.. include_body

:ref:`MPI_Group_excl` |mdash| Produces a group by reordering an existing group and
taking only unlisted members.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_excl.rst

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
