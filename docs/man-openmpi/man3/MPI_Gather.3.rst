.. _mpi_gather:

MPI_Gather
==========

.. include_body

:ref:`MPI_Gather`, :ref:`MPI_Igather`, :ref:`MPI_Gather_init` - Gathers values from a group
of processes.

Synopsis
--------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
       void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
       MPI_Comm comm)

   int MPI_Igather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
       void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
       MPI_Comm comm, MPI_Request *request)

   int MPI_Gather_init(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
       void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
       MPI_Comm comm, MPI_Info info, MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
           RECVTYPE, ROOT, COMM, IERROR)
       <type>  SENDBUF(*), RECVBUF(*)
       INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
       INTEGER COMM, IERROR

   MPI_IGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
           RECVTYPE, ROOT, COMM, REQUEST, IERROR)
       <type>  SENDBUF(*), RECVBUF(*)
       INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
       INTEGER COMM, REQUEST, IERROR

   MPI_GATHER_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
           RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR)
       <type>  SENDBUF(*), RECVBUF(*)
       INTEGER SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
       INTEGER COMM, INFO, REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Gather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
           root, comm, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
       TYPE(*), DIMENSION(..) :: recvbuf
       INTEGER, INTENT(IN) :: sendcount, recvcount, root
       TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Igather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
           root, comm, request, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
       INTEGER, INTENT(IN) :: sendcount, recvcount, root
       TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Request), INTENT(OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Gather_init(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
           root, comm, info, request, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
       INTEGER, INTENT(IN) :: sendcount, recvcount, root
       TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Info), INTENT(IN) :: info
       TYPE(MPI_Request), INTENT(OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``sendbuf`` : Starting address of send buffer (choice).
* ``sendcount`` : Number of elements in send buffer (integer).
* ``sendtype`` : Datatype of send buffer elements (handle).
* ``recvcount`` : Number of elements for any single receive (integer,
  significant only at root).
* ``recvtype`` : Datatype of recvbuffer elements (handle, significant only
  at root).
* ``root`` : Rank of receiving process (integer).
* ``comm`` : Communicator (handle).
* ``info`` : Info (handle, persistent only).

OUTPUT PARAMETERS
-----------------

* ``recvbuf`` : Address of receive buffer (choice, significant only at
   root).
* ``request`` : Request (handle, non-blocking only).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Each process (root process included) sends the contents of its send
buffer to the root process. The root process receives the messages and
stores them in rank order. The outcome is as if each of the n processes
in the group (including the root process) had executed a call to

::
   
   MPI_Send(sendbuf, sendcount, sendtype, root, ...)

and the root had executed n calls to

::
   
   MPI_Recv(recfbuf + i * recvcount * extent(recvtype), recvcount,
            recvtype, i, ...)

where extent(recvtype) is the type extent obtained from a call to
:ref:`MPI_Type_extent`.

An alternative description is that the n messages sent by the processes
in the group are concatenated in rank order, and the resulting message
is received by the root as if by a call to
``MPI_Recv(recvbuf, recvcount * n, recvtype, ... )``.

The receive buffer is ignored for all nonroot processes.

General, derived datatypes are allowed for both sendtype and recvtype.
The type signature of sendcount, sendtype on process i must be equal to
the type signature of recvcount, recvtype at the root. This implies that
the amount of data sent must be equal to the amount of data received,
pairwise between each process and the root. Distinct type maps between
sender and receiver are still allowed.

All arguments to the function are significant on process root, while on
other processes, only arguments sendbuf, sendcount, sendtype, root, comm
are significant. The arguments root and comm must have identical values
on all processes.

The specification of counts and types should not cause any location on
the root to be written more than once. Such a call is erroneous. Note
that the recvcount argument at the root indicates the number of items it
receives from each process, not the total number of items it receives.

Example 1: Gather 100 ints from every process in group to root.

.. code-block:: c
   
   MPI_Comm comm;
   int gsize, sendarray[100];
   int root, *rbuf;
   ...

   MPI_Comm_size( comm, &gsize);
   rbuf = (int*)malloc(gsize* 100*sizeof(int));

   MPI_Gather( sendarray, 100, MPI_INT, rbuf, 100, MPI_INT, root, comm);

Example 2: Previous example modified |mdash| only the root allocates memory
for the receive buffer.

.. code-block:: c
   
   MPI_Comm comm;
   int gsize, sendarray[100];
   int root, myrank, *rbuf;
   ...

   MPI_Comm_rank( comm, myrank);
   if ( myrank == root) {
     MPI_Comm_size( comm, &gsize);
     rbuf = (int *)malloc(gsize * 100*sizeof(int));
   }
   MPI_Gather( sendarray, 100, MPI_INT, rbuf, 100, MPI_INT, root, comm);

Example 3: Do the same as the previous example, but use a derived
datatype. Note that the type cannot be the entire set of ``gsize * 100``
ints since type matching is defined pairwise between the root and each
process in the gather.

.. code-block:: c

   MPI_Comm comm;
   int gsize, sendarray[100];
   int root, *rbuf;
   MPI_Datatype rtype;
   ...
   
   MPI_Comm_size( comm, &gsize);
   MPI_Type_contiguous( 100, MPI_INT, &rtype);
   MPI_Type_commit( &rtype );
   rbuf = (int*)malloc(gsize* 100*sizeof(int));
   MPI_Gather( sendarray, 100, MPI_INT, rbuf, 1, rtype, root, comm);


Use Of In-Place Option
----------------------

When the communicator is an intracommunicator, you can perform a gather
operation in-place (the output buffer is used as the input buffer). Use
the variable MPI_IN_PLACE as the value of the root process sendbuf. In
this case, sendcount and sendtype are ignored, and the contribution of
the root process to the gathered vector is assumed to already be in the
correct place in the receive buffer. Note that MPI_IN_PLACE is a special
kind of value; it has the same restrictions on its use as MPI_BOTTOM.
Because the in-place option converts the receive buffer into a
send-and-receive buffer, a Fortran binding that includes INTENT must
mark these as INOUT, not OUT.

When Communicator Is An Inter-Communicator
------------------------------------------

When the communicator is an inter-communicator, the root process in the
first group gathers data from all the processes in the second group. The
first group defines the root process. That process uses MPI_ROOT as the
value of its root argument. The remaining processes use ``MPI_PROC_NULL`` as
the value of their root argument. All processes in the second group use
the rank of that root process in the first group as the value of their
root argument. The send buffer argument of the processes in the first
group must be consistent with the receive buffer argument of the root
process in the second group.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Gatherv`
