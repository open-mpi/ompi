.. _mpi_group_compare:

MPI_Group_compare
=================

.. include_body

:ref:`MPI_Group_compare` |mdash| Compares two groups.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_compare(MPI_Group group1, MPI_Group group2,
       int *result)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GROUP_COMPARE(GROUP1, GROUP2, RESULT, IERROR)
       INTEGER GROUP1, GROUP2, RESULT, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Group_compare(group1, group2, result, ierror)
       TYPE(MPI_Group), INTENT(IN) :: group1, group2
       INTEGER, INTENT(OUT) :: result
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
