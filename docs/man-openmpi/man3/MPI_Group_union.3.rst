.. _mpi_group_union:


MPI_Group_union
===============

.. include_body

:ref:`MPI_Group_union` |mdash| Produces a group by combining two groups.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_union(MPI_Group group1, MPI_Group group2,
   	MPI_Group *newgroup)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_GROUP_UNION(GROUP1, GROUP2, NEWGROUP, IERROR)
   	INTEGER	GROUP1, GROUP2, NEWGROUP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Group_union(group1, group2, newgroup, ierror)
   	TYPE(MPI_Group), INTENT(IN) :: group1, group2
   	TYPE(MPI_Group), INTENT(OUT) :: newgroup
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``group1``: First group (handle).
* ``group2``: Second group (handle).

OUTPUT PARAMETERS
-----------------
* ``newgroup``: Union group (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The set-like operations are defined as follows:

 o    union |mdash| All elements of the first group (group1), followed by all elements of second group (group2) not in first.

 o    intersect |mdash| all elements of the first group that are also in the second group, ordered as in first group.

 o    difference |mdash| all elements of the first group that are not in the second group, ordered as in the first group.

Note that for these operations the order of processes in the output
group is determined primarily by order in the first group (if possible)
and then, if necessary, by order in the second group. Neither union nor
intersection are commutative, but both are associative.

The new group can be empty, that is, equal to MPI_GROUP_EMPTY.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Group_free`
