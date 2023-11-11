.. _mpi_comm_create_group:

MPI_Comm_create_group
=====================

.. include_body

:ref:`MPI_Comm_create_group` |mdash| Creates a new communicator.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm *newcomm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_CREATE_GROUP(COMM, GROUP, TAG, NEWCOMM, IERROR)
       INTEGER COMM, GROUP, TAG, NEWCOMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Comm_create_group(comm, group, tag, newcomm, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Group), INTENT(IN) :: group
       INTEGER, INTENT(IN) :: tag
       TYPE(MPI_Comm), INTENT(OUT) :: newcomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``comm`` : Communicator (handle).
* ``group`` : Group, which is a subset of the group of comm (handle).
* ``tag`` : Tag (integer).

OUTPUT PARAMETERS
-----------------

* ``newcomm`` : New communicator (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_create_group` is similar to :ref:`MPI_Comm_create`; however,
:ref:`MPI_Comm_create` must be called by all processes in the group of comm,
whereas :ref:`MPI_Comm_create_group` must be called by all processes in group,
which is a subgroup of the group of comm. In addition,
:ref:`MPI_Comm_create_group` requires that comm is an intracommunicator.
:ref:`MPI_Comm_create_group` returns a new intracommunicator, newcomm, for
which the group argument defines the communication group. No cached
information propagates from comm to newcomm. Each process must provide a
group argument that is a subgroup of the group associated with comm;
this could be MPI_GROUP_EMPTY. If a non-empty group is specified, then
all processes in that group must call the function, and each of these
processes must provide the same arguments, including a group that
contains the same members with the same ordering. Otherwise the call is
erroneous. If the calling process is a member of the group given as the
group argument, then newcomm is a communicator with group as its
associated group. If the calling process is not a member of group, e.g.,
group is MPI_GROUP_EMPTY, then the call is a local operation and
MPI_COMM_NULL is returned as newcomm.

NOTES
-----

:ref:`MPI_Comm_create_group` provides a means of making a subset of processes
for the purpose of separate MIMD computation, with separate
communication space. newcomm, which is created by :ref:`MPI_Comm_create_group`,
can be used in subsequent calls to :ref:`MPI_Comm_create_group` (or other
communicator constructors) to further subdivide a computation into
parallel sub-computations. A more general service is provided by
:ref:`MPI_Comm_split`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create`
