.. _mpi_group_range_excl:


MPI_Group_range_excl
====================

.. include_body

:ref:`MPI_Group_range_excl` |mdash| Produces a group by excluding ranges of
processes from an existing group.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_range_excl(MPI_Group group, int n, int ranges[][3],
   	MPI_Group *newgroup)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_GROUP_RANGE_EXCL(GROUP, N, RANGES, NEWGROUP, IERROR)
   	INTEGER	GROUP, N, RANGES(3,*), NEWGROUP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Group_range_excl(group, n, ranges, newgroup, ierror)
   	TYPE(MPI_Group), INTENT(IN) :: group
   	INTEGER, INTENT(IN) :: n, ranges(3,n)
   	TYPE(MPI_Group), INTENT(OUT) :: newgroup
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``group``: Group (handle).
* ``n``: Number of triplets in array ranges (integer).
* ``ranges``: A one-dimensional array of integer triplets of the form (first rank, last rank, stride), indicating the ranks in group of processes to be excluded from the output group newgroup.

OUTPUT PARAMETERS
-----------------
* ``newgroup``: New group derived from above, preserving the order in group (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Each computed rank must be a valid rank in group and all computed ranks
must be distinct, or else the program is erroneous.

The functionality of this routine is specified to be equivalent to
expanding the array of ranges to an array of the excluded ranks and
passing the resulting array of ranks and other arguments to
:ref:`MPI_Group_excl`. A call to :ref:`MPI_Group_excl` is equivalent to a call to
:ref:`MPI_Group_range_excl` with each rank i in ranks replaced by the triplet
(i,i,1) in the argument ranges.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Group_excl`
   * :ref:`MPI_Group_free`
