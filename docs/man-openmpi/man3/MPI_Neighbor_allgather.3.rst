.. _mpi_neighbor_allgather:


MPI_Neighbor_allgather
======================

.. include_body

:ref:`MPI_Neighbor_allgather`, :ref:`MPI_Ineighbor_allgather`,
:ref:`MPI_Neighbor_allgather` |mdash| Gathers and distributes data from and to all
neighbors


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Neighbor_allgather(const void *sendbuf, int  sendcount,
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	 MPI_Datatype recvtype, MPI_Comm comm)

   int MPI_Ineighbor_allgather(const void *sendbuf, int  sendcount,
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	 MPI_Datatype recvtype, MPI_Comm comm, MPI_Request req)

   int MPI_Neighbor_allgather_init(const void *sendbuf, int  sendcount,
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	 MPI_Datatype recvtype, MPI_Comm comm, MPI_Infoinfo, MPI_Request req)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_NEIGHBOR_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
   		RECVTYPE, COMM, IERROR)
   	<type>	SENDBUF (*), RECVBUF (*)
   	INTEGER	SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM,
   	INTEGER	IERROR

   MPI_INEIGHBOR_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
   		RECVTYPE, COMM, REQUEST, IERROR)
   	<type>	SENDBUF (*), RECVBUF (*)
   	INTEGER	SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM,
   	INTEGER	REQUEST, IERROR

   MPI_NEIGHBOR_ALLGATHER_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
   		RECVTYPE, COMM, INFO, IREQUEST, IERROR)
   	<type>	SENDBUF (*), RECVBUF (*)
   	INTEGER	SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, COMM,
   	INTEGER	INFO, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Neighbor_allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount,
   		recvtype, comm, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
   	TYPE(*), DIMENSION(..) :: recvbuf
   	INTEGER, INTENT(IN) :: sendcount, recvcount
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Ineighbor_allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount,
   		recvtype, comm, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN) :: sendcount, recvcount
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Neighbor_allgather_init(sendbuf, sendcount, sendtype, recvbuf, recvcount,
   		recvtype, comm, info, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN) :: sendcount, recvcount
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``sendbuf``: Starting address of send buffer (choice).
* ``sendcount``: Number of elements in send buffer (integer).
* ``sendtype``: Datatype of send buffer elements (handle).
* ``recvbuf``: Starting address of recv buffer (choice).
* ``recvcount``: Number of elements received from any process (integer).
* ``recvtype``: Datatype of receive buffer elements (handle).
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Address of receive buffer (choice).
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Neighbor_allgather` is similar to :ref:`MPI_Allgather`, except that only the
neighboring processes receive the result, instead of all processes. The
neighbors and buffer layout is determined by the topology of *comm*.

The type signature associated with sendcount, sendtype at a process must
be equal to the type signature associated with recvcount, recvtype at
any other process.


NEIGHBOR ORDERING
-----------------

For a distributed graph topology, created with :ref:`MPI_Dist_graph_create`,
the sequence of neighbors in the send and receive buffers at each
process is defined as the sequence returned by :ref:`MPI_Dist_graph_neighbors`
for destinations and sources, respectively. For a general graph
topology, created with :ref:`MPI_Graph_create`, the order of neighbors in the
send and receive buffers is defined as the sequence of neighbors as
returned by :ref:`MPI_Graph_neighbors`. Note that general graph topologies
should generally be replaced by the distributed graph topologies.

For a Cartesian topology, created with :ref:`MPI_Cart_create`, the sequence of
neighbors in the send and receive buffers at each process is defined by
order of the dimensions, first the neighbor in the negative direction
and then in the positive direction with displacement 1. The numbers of
sources and destinations in the communication routines are 2*ndims with
ndims defined in :ref:`MPI_Cart_create`. If a neighbor does not exist, i.e., at
the border of a Cartesian topology in the case of a non-periodic virtual
grid dimension (i.e., periods[...]==false), then this neighbor is
defined to be ``MPI_PROC_NULL``.

If a neighbor in any of the functions is ``MPI_PROC_NULL``, then the
neighborhood collective communication behaves like a point-to-point
communication with ``MPI_PROC_NULL`` in this direction. That is, the buffer
is still part of the sequence of neighbors but it is neither
communicated nor updated.


NOTES
-----

The MPI_IN_PLACE option for *sendbuf* is not meaningful for this
operation.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Neighbor_allgatherv`
   * :ref:`MPI_Cart_create`
   * MPI_Garph_create
   * :ref:`MPI_Dist_graph_create`
   * :ref:`MPI_Gather`
