.. _mpi_group_compare:

MPI_Group_compare
=================

.. include_body

:ref:`MPI_Group_compare` |mdash| Compares two groups.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_compare.rst

INPUT PARAMETERS
----------------

* ``group1`` : First group (handle).
* ``group2`` : Second group (handle).

OUTPUT PARAMETERS
-----------------

* ``result`` : Integer which is MPI_IDENT if the order and members of
   the two groups are the same, MPI_SIMILAR if only the members are the
   same, and MPI_UNEQUAL otherwise.
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

MPI_IDENT results if the group members and group order is exactly
the same in both groups. This happens for instance if ``group1`` and
``group2`` are the same handle. MPI_SIMILAR results if the group
members are the same but the order is different. MPI_UNEQUAL results
otherwise.

ERRORS
------

.. include:: ./ERRORS.rst
