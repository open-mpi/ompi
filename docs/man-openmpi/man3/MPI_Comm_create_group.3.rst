.. _mpi_comm_create_group:

MPI_Comm_create_group
=====================

.. include_body

:ref:`MPI_Comm_create_group` - Creates a new communicator.

Syntax
------

C Syntax
^^^^^^^^

.. code:: C

   #include <mpi.h>

   int MPI_Comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm *newcomm)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_COMM_CREATE_GROUP(COMM, GROUP, TAG, NEWCOMM, IERROR)
       INTEGER COMM, GROUP, TAG, NEWCOMM, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: Fortran

   USE mpi_f08

   MPI_Comm_create_group(comm, group, tag, newcomm, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Group), INTENT(IN) :: group
       INTEGER, INTENT(IN) :: tag
       TYPE(MPI_Comm), INTENT(OUT) :: newcomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  comm : Communicator (handle).
-  group : Group, which is a subset of the group of comm (handle).
-  tag : Tag (integer).

Output Parameters
-----------------

-  newcomm : New communicator (handle).
-  IERROR : Fortran only: Error status (integer).

Description
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

Notes
-----

:ref:`MPI_Comm_create_group` provides a means of making a subset of processes
for the purpose of separate MIMD computation, with separate
communication space. newcomm, which is created by :ref:`MPI_Comm_create_group`,
can be used in subsequent calls to :ref:`MPI_Comm_create_group` (or other
communicator constructors) to further subdivide a computation into
parallel sub-computations. A more general service is provided by
:ref:`MPI_Comm_split`.

Errors
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument. Before the
error value is returned, the current MPI error handler is called. By
default, this error handler aborts the MPI job, except for I/O function
errors. The error handler may be changed with :ref:`MPI_Comm_set_errhandler`;
the predefined error handler MPI_ERRORS_RETURN may be used to cause
error values to be returned. Note that MPI does not guarantee that an
MPI program can continue past an error.


.. seealso:: :ref:`MPI_Comm_create`
