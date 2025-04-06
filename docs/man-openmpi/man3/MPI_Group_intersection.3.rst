.. _mpi_group_intersection:

MPI_Group_intersection
======================

.. include_body

:ref:`MPI_Group_intersection` |mdash| Produces a group at the intersection of two
existing groups.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_intersection.rst

INPUT PARAMETERS
----------------

* ``group1`` : First group (handle).
* ``group2`` : Second group (handle).

OUTPUT PARAMETERS
-----------------

* ``newgroup`` : Intersection group (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

The set-like operations are defined as follows:

* union |mdash| All elements of the first group (group1), followed by all
   elements of second group (group2) not in first.
* intersect |mdash| all elements of the first group that are also in the
   second group, ordered as in first group.
* difference |mdash| all elements of the first group that are not in the
   second group, ordered as in the first group.

Note that for these operations the order of processes in the output
group is determined primarily by order in the first group (if possible)
and then, if necessary, by order in the second group. Neither union nor
intersection are commutative, but both are associative.

The new group can be empty, that is, equal to MPI_GROUP_EMPTY.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Group_free`
