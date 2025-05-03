.. _mpi_comm_create:

MPI_Comm_create
===============

.. include_body

:ref:`MPI_Comm_create` |mdash| Creates a new communicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_create.rst

INPUT PARAMETER
---------------

* ``comm`` : Communicator (handle).
* ``group`` : Group, which is a subset of the group of comm (handle).

OUTPUT PARAMETERS
-----------------

* ``newcomm`` : New communicator (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This function creates a new communicator newcomm with communication
group defined by group and a new context. The function sets newcomm to a
new communicator that spans all the processes that are in the group. It
sets newcomm to MPI_COMM_NULL for processes that are not in the group.
Each process must call with a group argument that is a subgroup of the
group associated with comm; this could be MPI_GROUP_EMPTY. The processes
may specify different values for the group argument. If a process calls
with a non-empty group, then all processes in that group must call the
function with the same group as argument, that is: the same processes in
the same order. Otherwise the call is erroneous.

NOTES
-----

:ref:`MPI_Comm_create` provides a means of making a subset of processes for the
purpose of separate MIMD computation, with separate communication space.
newcomm, which is created by :ref:`MPI_Comm_create`, can be used in subsequent
calls to :ref:`MPI_Comm_create` (or other communicator constructors) to further
subdivide a computation into parallel sub-computations. A more general
service is provided by :ref:`MPI_Comm_split`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_split`
