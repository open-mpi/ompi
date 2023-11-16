.. _mpi_comm_compare:

MPI_Comm_compare
================

.. include_body

:ref:`MPI_Comm_compare` |mdash| Compares two communicators.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_COMPARE(COMM1, COMM2, RESULT, IERROR)
       INTEGER COMM1, COMM2, RESULT, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Comm_compare(comm1, comm2, result, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm1, comm2
       INTEGER, INTENT(OUT) :: result
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``comm1`` : Comm1 (handle).
* ``comm2`` : Comm2 (handle).

OUTPUT PARAMETERS
-----------------

* ``result`` : Result of comparison (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

MPI_IDENT ``result``\ s if and only if ``comm1`` and ``comm2`` are
handles for the same object (identical groups and same contexts).
MPI_CONGRUENT results if the underlying groups are identical in
constituents and rank order; these communicators differ only by context.
MPI_SIMILAR results of the group members of both communicators are
the same but the rank order differs. MPI_UNEQUAL results otherwise.

ERRORS
------

.. include:: ./ERRORS.rst
