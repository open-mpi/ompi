.. _mpi_neighbor_alltoall:


MPI_Neighbor_alltoall
=====================

.. include_body

:ref:`MPI_Neighbor_alltoall`, :ref:`MPI_Ineighbor_alltoall`, :ref:`MPI_Neighbor_alltoall` - All processes send data to neighboring processes in a virtual topology communicator


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Neighbor_alltoall(const void *sendbuf, int sendcount,
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	MPI_Datatype recvtype, MPI_Comm comm)

   int MPI_Ineighbor_alltoall(const void *sendbuf, int sendcount,
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request)

   int MPI_Neighbor_alltoall_init(const void *sendbuf, int sendcount,
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_NEIGHBOR_ALLTOALL(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
   	RECVTYPE, COMM, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
   	INTEGER	COMM, IERROR

   MPI_INEIGHBOR_ALLTOALL(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
   	RECVTYPE, COMM, REQUEST, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
   	INTEGER	COMM, REQUEST, IERROR

   MPI_NEIGHBOR_ALLTOALL_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT,
   	RECVTYPE, COMM, INFO, REQUEST, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE
   	INTEGER	COMM, INFO, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Neighbor_alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount,
   		recvtype, comm, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
   	TYPE(*), DIMENSION(..) :: recvbuf
   	INTEGER, INTENT(IN) :: sendcount, recvcount
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Ineighbor_alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount,
   		recvtype, comm, request, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN) :: sendcount, recvcount
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Neighbor_alltoall_init(sendbuf, sendcount, sendtype, recvbuf, recvcount,
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
* ``sendcount``: Number of elements to send to each process (integer).
* ``sendtype``: Datatype of send buffer elements (handle).
* ``recvcount``: Number of elements to receive from each process (integer).
* ``recvtype``: Datatype of receive buffer elements (handle).
* ``comm``: Communicator over which data is to be exchanged (handle).
* ``info``: Info (handle, persistent only).

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Starting address of receive buffer (choice).
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Neighbor_alltoall` is a collective operation in which all processes
send and receive the same amount of data to each neighbor. The operation
of this routine can be represented as follows, where each process
performs 2n (n being the number of neighbors in communicator *comm*)
independent point-to-point communications. The neighbors and buffer
layout are determined by the topology of *comm*.

Example of :ref:`MPI_Neighbor_alltoall` semantics for cartesian topologies:

.. code-block:: c

           MPI_Cart_get(comm, maxdims, dims, periods, coords);
           for (dim = 0, i = 0 ; dim < dims ; ++dim) {
               MPI_Cart_shift(comm, dim, 1, &r0, &r1);
               MPI_Isend(sendbuf + i * sendcount * extent(sendtype),
                         sendcount, sendtype, r0, ..., comm, ...);
               MPI_Irecv(recvbuf + i * recvcount * extent(recvtype),
                         recvcount, recvtype, r0, ..., comm, ...);
               ++i;
               MPI_Isend(sendbuf + i * sendcount * extent(sendtype),
                         sendcount, sendtype, r1, ..., comm, &req[i]);
               MPI_Irecv(recvbuf + i * recvcount * extent(recvtype),
                         recvcount, recvtype, r1, ..., comm, ...);
               ++i;
           }

           MPI_Waitall (...);

Each process breaks up its local *sendbuf* into n blocks - each
containing *sendcount* elements of type *sendtype* - and divides its
*recvbuf* similarly according to *recvcount* and *recvtype*. Process j
sends the k-th block of its local *sendbuf* to neighbor k, which places
the data in the j-th block of its local *recvbuf*. The amount of data
sent must be equal to the amount of data received, pairwise, between
every pair of processes.


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
function.

All arguments on all processes are significant. The *comm* argument, in
particular, must describe the same communicator on all processes. *comm*
must be either a cartesian, graph, or dist graph communicator.

There are two MPI library functions that are more general than
:ref:`MPI_Neighbor_alltoall`. :ref:`MPI_Neighbor_alltoallv` allows all-to-all
communication to and from buffers that need not be contiguous; different
processes may send and receive different amounts of data.
:ref:`MPI_Neighbor_alltoallw` expands :ref:`MPI_Neighbor_alltoallv`'s functionality to
allow the exchange of data with different datatypes.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Neighbor_alltoallv`
   * :ref:`MPI_Neighbor_alltoallw`
   * :ref:`MPI_Cart_create`
   * :ref:`MPI_Graph_create`
   * :ref:`MPI_Dist_graph_create`
   * :ref:`MPI_Dist_graph_create_adjacent`
