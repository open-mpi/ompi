.. _mpi_comm_split:


MPI_Comm_split
==============

.. include_body

:ref:`MPI_Comm_split` |mdash| Creates new communicators based on colors and keys.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_split(MPI_Comm comm, int color, int key,
   	MPI_Comm *newcomm)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_SPLIT(COMM, COLOR, KEY, NEWCOMM, IERROR)
   	INTEGER	COMM, COLOR, KEY, NEWCOMM, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_split(comm, color, key, newcomm, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(IN) :: color, key
   	TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``comm``: Communicator (handle).
* ``color``: Control of subset assignment (nonnegative integer).
* ``key``: Control of rank assignment (integer).

OUTPUT PARAMETERS
-----------------
* ``newcomm``: New communicator (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function partitions the group associated with comm into disjoint
subgroups, one for each value of color. Each subgroup contains all
processes of the same color. Within each subgroup, the processes are
ranked in the order defined by the value of the argument key, with ties
broken according to their rank in the old group. A new communicator is
created for each subgroup and returned in newcomm. A process may supply
the color value MPI_UNDEFINED, in which case newcomm returns
MPI_COMM_NULL. This is a collective call, but each process is permitted
to provide different values for color and key.

When you call :ref:`MPI_Comm_split` on an inter-communicator, the processes on
the left with the same color as those on the right combine to create a
new inter-communicator. The key argument describes the relative rank of
processes on each side of the inter-communicator. The function returns
MPI_COMM_NULL for those colors that are specified on only one side of
the inter-communicator, or for those that specify MPI_UNDEFINED as the
color.

A call to MPI_Comm_create(comm, *group*, *newcomm*) is equivalent to a
call to MPI_Comm_split(comm, *color*,\ *key*, *newcomm*), where all
members of *group* provide *color* = 0 and *key* = rank in group, and
all processes that are not members of *group* provide *color* =
MPI_UNDEFINED. The function :ref:`MPI_Comm_split` allows more general
partitioning of a group into one or more subgroups with optional
reordering.

The value of *color* must be nonnegative or MPI_UNDEFINED.


NOTES
-----

This is an extremely powerful mechanism for dividing a single
communicating group of processes into k subgroups, with k chosen
implicitly by the user (by the number of colors asserted over all the
processes). Each resulting communicator will be nonoverlapping. Such a
division could be useful for defining a hierarchy of computations, such
as for multigrid or linear algebra.

Multiple calls to :ref:`MPI_Comm_split` can be used to overcome the requirement
that any call have no overlap of the resulting communicators (each
process is of only one color per call). In this way, multiple
overlapping communication structures can be created. Creative use of the
color and key in such splitting operations is encouraged.

Note that, for a fixed color, the keys need not be unique. It is
:ref:`MPI_Comm_split`'s responsibility to sort processes in ascending order
according to this key, and to break ties in a consistent way. If all the
keys are specified in the same way, then all the processes in a given
color will have the relative rank order as they did in their parent
group. (In general, they will have different ranks.)

Essentially, making the key value zero for all processes of a given
color means that one needn't really pay attention to the rank-order of
the processes in the new communicator.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create`
   * :ref:`MPI_Intercomm_create`
   * :ref:`MPI_Comm_dup`
   * :ref:`MPI_Comm_free`
