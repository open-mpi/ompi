.. _mpi_group_rank:


MPI_Group_rank
==============

.. include_body

:ref:`MPI_Group_rank` |mdash| Returns the rank of the calling process in the
given group.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_rank(MPI_Group group, int *rank)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_GROUP_RANK(GROUP, RANK, IERROR)
   	INTEGER	GROUP, RANK, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Group_rank(group, rank, ierror)
   	TYPE(MPI_Group), INTENT(IN) :: group
   	INTEGER, INTENT(OUT) :: rank
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``group``: Group (handle).

OUTPUT PARAMETERS
-----------------
* ``rank``: Rank of the calling process in group, or MPI_UNDEFINED if the process is not a member (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Group_rank` returns as the output parameter *rank* the rank of the
calling process in group. If the process is not a member of group then
MPI_UNDEFINED is returned.


ERRORS
------

.. include:: ./ERRORS.rst
