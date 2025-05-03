.. _mpi_group_incl:

MPI_Group_incl
==============

.. include_body

:ref:`MPI_Group_incl` |mdash| Produces a group by reordering an existing group and
taking only listed members.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_incl.rst

INPUT PARAMETERS
----------------

* ``group`` : Group (handle).
* ``n`` : Number of elements in array ranks (and size of
   newgroup)(integer).
* ``ranks`` : Ranks of processes in group to appear in newgroup (array of
   integers).

OUTPUT PARAMETERS
-----------------

* ``newgroup`` : New group derived from above, in the order defined by
   ranks (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Group_incl` creates a group group_out that consists of
the n processes in group with ranks rank[0], ..., rank[n-1]; the process
with rank i in group_out is the process with rank ranks[i] in group.
Each of the n elements of ranks must be a valid rank in group and all
elements must be distinct, or else the program is erroneous. If n = 0,
then group_out is MPI_GROUP_EMPTY. This function can, for instance, be
used to reorder the elements of a group.

NOTE
----

This implementation does not currently check to ensure that there are no
duplicates in the list of ranks.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Group_compare`
