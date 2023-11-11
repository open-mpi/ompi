.. _mpi_group_range_incl:


MPI_Group_range_incl
====================

.. include_body

:ref:`MPI_Group_range_incl` |mdash| Creates a new group from ranges of ranks in
an existing group.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_range_incl(MPI_Group group, int n, int ranges[][3],
   	MPI_Group *newgroup)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_GROUP_RANGE_INCL(GROUP, N, RANGES, NEWGROUP, IERROR)
   	INTEGER	GROUP, N, RANGES(3,*), NEWGROUP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Group_range_incl(group, n, ranges, newgroup, ierror)
   	TYPE(MPI_Group), INTENT(IN) :: group
   	INTEGER, INTENT(IN) :: n, ranges(3,n)
   	TYPE(MPI_Group), INTENT(OUT) :: newgroup
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``group``: Group (handle).
* ``n``: Number of triplets in array ranges (integer).
* ``ranges``: A one-dimensional array of integer triplets, of the form (first rank, last rank, stride) indicating ranks in group or processes to be included in newgroup.

OUTPUT PARAMETERS
-----------------
* ``newgroup``: New group derived from above, in the order defined by ranges (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

If ranges consist of the triplets

::

       (first1, last1, stride1), ..., (firstn, lastn, striden)

then newgroup consists of the sequence of processes in group with ranks

::

                                                    last(1)-first(1)
     first(1), first(1) + stride(1),..., first(1) + ---------------- stride(1),...
                                                        stride(1)

                                                    last(n)-first(n)
     first(n), first(n) + stride(n),..., first(n) + ---------------- stride(n).
                                                        stride(n)

Each computed rank must be a valid rank in group and all computed ranks
must be distinct, or else the program is erroneous. Note that we may
have first(i) > last(i), and stride(i) may be negative, but cannot be
zero.

The functionality of this routine is specified to be equivalent to
expanding the array of ranges to an array of the included ranks and
passing the resulting array of ranks and other arguments to
:ref:`MPI_Group_incl`. A call to :ref:`MPI_Group_incl` is equivalent to a call to
:ref:`MPI_Group_range_incl` with each rank i in ranks replaced by the triplet
(i,i,1) in the argument ranges.


NOTE
----

This implementation does not currently check to see that the list of
ranges to include are valid ranks in the group.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Group_incl`
   * :ref:`MPI_Group_free`
