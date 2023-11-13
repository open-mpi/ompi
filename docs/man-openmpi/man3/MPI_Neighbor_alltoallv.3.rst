.. _mpi_neighbor_alltoallv:


MPI_Neighbor_alltoallv
======================

.. include_body

:ref:`MPI_Neighbor_alltoallv`, :ref:`MPI_Ineighbor_alltoallv`,
:ref:`MPI_Neighbor_alltoallv_init` |mdash| All processes send different amounts of
data to, and receive different amounts of data from, all neighbors


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Neighbor_alltoallv(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], MPI_Datatype sendtype,
   	void *recvbuf, const int recvcounts[],
   	const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm)

   int MPI_Ineighbor_alltoallv(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], MPI_Datatype sendtype,
   	void *recvbuf, const int recvcounts[],
   	const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
   	MPI_Request *request)

   int MPI_Neighbor_alltoallv_init(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], MPI_Datatype sendtype,
   	void *recvbuf, const int recvcounts[],
   	const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
   	MPI_Info info, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_NEIGHBOR_ALLTOALLV(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPE
   	INTEGER	COMM, IERROR

   MPI_INEIGHBOR_ALLTOALLV(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, REQUEST, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPE
   	INTEGER	COMM, REQUEST, IERROR

   MPI_NEIGHBOR_ALLTOALLV_INIT(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, INFO, REQUEST, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPE
   	INTEGER	COMM, INFO, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Neighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf,
   		recvcounts, rdispls, recvtype, comm, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
   	TYPE(*), DIMENSION(..) :: recvbuf
   	INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*),
   	rdispls(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Ineighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf,
   		recvcounts, rdispls, recvtype, comm, request, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*),
   	recvcounts(*), rdispls(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Neighbor_alltoallv_init(sendbuf, sendcounts, sdispls, sendtype, recvbuf,
   		recvcounts, rdispls, recvtype, comm, info, request, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*),
   	recvcounts(*), rdispls(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``sendbuf``: Starting address of send buffer.
* ``sendcounts``: Integer array, where entry i specifies the number of elements to send to neighbor i.
* ``sdispls``: Integer array, where entry i specifies the displacement (offset from *sendbuf*, in units of *sendtype*) from which to send data to neighbor i.
* ``sendtype``: Datatype of send buffer elements.
* ``recvcounts``: Integer array, where entry j specifies the number of elements to receive from neighbor j.
* ``rdispls``: Integer array, where entry j specifies the displacement (offset from *recvbuf*, in units of *recvtype*) to which data from neighbor j should be written.
* ``recvtype``: Datatype of receive buffer elements.
* ``comm``: Communicator over which data is to be exchanged.
* ``info``: Info (handle, persistent only).

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Address of receive buffer.
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status.

DESCRIPTION
-----------

:ref:`MPI_Neighbor_alltoallv` is a generalized collective operation in which
all processes send data to and receive data from all neighbors. It adds
flexibility to :ref:`MPI_Neighbor_alltoall` by allowing the user to specify
data to send and receive vector-style (via a displacement and element
count). The operation of this routine can be thought of as follows,
where each process performs 2n (n being the number of neighbors in to
topology of communicator *comm*) independent point-to-point
communications. The neighbors and buffer layout are determined by the
topology of *comm*.

.. code-block:: c

           MPI_Cart_get(comm, maxdims, dims, periods, coords);
           for (dim = 0, i = 0 ; dim < dims ; ++dim) {
               MPI_Cart_shift(comm, dim, 1, &r0, &r1);
               MPI_Isend(sendbuf + sdispls[i]  * extent(sendtype),
                         sendcount, sendtype, r0, ..., comm, ...);
               MPI_Irecv(recvbuf + rdispls[i] * extent(recvtype),
                         recvcount, recvtype, r0, ..., comm, ...);
               ++i;
               MPI_Isend(sendbuf + sdispls[i] * extent(sendtype),
                         sendcount, sendtype, r1, ..., comm, &req[i]);
               MPI_Irecv(recvbuf + rdispls[i] * extent(recvtype),
                         recvcount, recvtype, r1, ..., comm, ...);
               ++i;
           }

Process j sends the k-th block of its local *sendbuf* to neighbor k,
which places the data in the j-th block of its local *recvbuf*.

When a pair of processes exchanges data, each may pass different element
count and datatype arguments so long as the sender specifies the same
amount of data to send (in bytes) as the receiver expects to receive.

Note that process i may send a different amount of data to process j
than it receives from process j. Also, a process may send entirely
different amounts of data to different processes in the communicator.


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

The specification of counts and displacements should not cause any
location to be written more than once.

All arguments on all processes are significant. The *comm* argument, in
particular, must describe the same communicator on all processes.

The offsets of *sdispls* and *rdispls* are measured in units of
*sendtype* and *recvtype*, respectively. Compare this to
:ref:`MPI_Neighbor_alltoallw`, where these offsets are measured in bytes.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Neighbor_alltoall`
   * :ref:`MPI_Neighbor_alltoallw`
   * :ref:`MPI_Cart_create`
   * :ref:`MPI_Graph_create`
   * :ref:`MPI_Dist_graph_create`
