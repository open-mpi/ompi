.. _mpi_alltoallv:


MPI_Alltoallv
=============

.. include_body

:ref:`MPI_Alltoallv`, :ref:`MPI_Ialltoallv`, :ref:`MPI_Alltoallv_init` - All processes
send different amount of data to, and receive different amount of data
from, all processes


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Alltoallv(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], MPI_Datatype sendtype,
   	void *recvbuf, const int recvcounts[],
   	const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm)

   int MPI_Ialltoallv(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], MPI_Datatype sendtype,
   	void *recvbuf, const int recvcounts[],
   	const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
   	MPI_Request *request)

   int MPI_Alltoallv_init(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], MPI_Datatype sendtype,
   	void *recvbuf, const int recvcounts[],
   	const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
   	MPI_Info info, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ALLTOALLV(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPE
   	INTEGER	COMM, IERROR

   MPI_IALLTOALLV(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, REQUEST, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPE
   	INTEGER	COMM, REQUEST, IERROR

   MPI_ALLTOALLV_INIT(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPE,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPE, COMM, INFO, REQUEST, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPE
   	INTEGER	COMM, INFO, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts,
   		rdispls, recvtype, comm, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
   	TYPE(*), DIMENSION(..) :: recvbuf
   	INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*),
   	rdispls(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Ialltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts,
   		rdispls, recvtype, comm, request, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*),
   	recvcounts(*), rdispls(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Alltoallv_init(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts,
   			rdispls, recvtype, comm, info, request, ierror)

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
* ``sendcounts``: Integer array, where entry i specifies the number of elements to send to rank i.
* ``sdispls``: Integer array, where entry i specifies the displacement (offset from *sendbuf*, in units of *sendtype*) from which to send data to rank i.
* ``sendtype``: Datatype of send buffer elements.
* ``recvcounts``: Integer array, where entry j specifies the number of elements to receive from rank j.
* ``rdispls``: Integer array, where entry j specifies the displacement (offset from *recvbuf*, in units of *recvtype*) to which data from rank j should be written.
* ``recvtype``: Datatype of receive buffer elements.
* ``comm``: Communicator over which data is to be exchanged.
* ``info``: Info (handle, persistent only)

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Address of receive buffer.
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status.

DESCRIPTION
-----------

:ref:`MPI_Alltoallv` is a generalized collective operation in which all
processes send data to and receive data from all other processes. It
adds flexibility to :ref:`MPI_Alltoall` by allowing the user to specify data to
send and receive vector-style (via a displacement and element count).
The operation of this routine can be thought of as follows, where each
process performs 2n (n being the number of processes in communicator
*comm*) independent point-to-point communications (including
communication with itself).

.. code-block:: c

   	MPI_Comm_size(comm, &n);
   	for (i = 0, i < n; i++)
   	    MPI_Send(sendbuf + sdispls[i] * extent(sendtype),
   	        sendcounts[i], sendtype, i, ..., comm);
   	for (i = 0, i < n; i++)
   	    MPI_Recv(recvbuf + rdispls[i] * extent(recvtype),
   	        recvcounts[i], recvtype, i, ..., comm);

Process j sends the k-th block of its local *sendbuf* to process k,
which places the data in the j-th block of its local *recvbuf*.

When a pair of processes exchanges data, each may pass different element
count and datatype arguments so long as the sender specifies the same
amount of data to send (in bytes) as the receiver expects to receive.

Note that process i may send a different amount of data to process j
than it receives from process j. Also, a process may send entirely
different amounts of data to different processes in the communicator.

WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR

When the communicator is an inter-communicator, the gather operation
occurs in two phases. The data is gathered from all the members of the
first group and received by all the members of the second group. Then
the data is gathered from all the members of the second group and
received by all the members of the first. The operation exhibits a
symmetric, full-duplex behavior.

The first group defines the root process. The root process uses MPI_ROOT
as the value of *root*. All other processes in the first group use
``MPI_PROC_NULL`` as the value of *root*. All processes in the second group
use the rank of the root process in the first group as the value of
*root*.

When the communicator is an intra-communicator, these groups are the
same, and the operation occurs in a single phase.


USE OF IN-PLACE OPTION
----------------------

When the communicator is an intracommunicator, you can perform an
all-to-all operation in-place (the output buffer is used as the input
buffer). Use the variable MPI_IN_PLACE as the value of *sendbuf*. In
this case, *sendcounts*, *sdispls*, and *sendtype* are ignored. The
input data of each process is assumed to be in the area where that
process would receive its own contribution to the receive buffer.


NOTES
-----

The specification of counts and displacements should not cause any
location to be written more than once.

All arguments on all processes are significant. The *comm* argument, in
particular, must describe the same communicator on all processes.

The offsets of *sdispls* and *rdispls* are measured in units of
*sendtype* and *recvtype*, respectively. Compare this to :ref:`MPI_Alltoallw`,
where these offsets are measured in bytes.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Alltoall`
   * :ref:`MPI_Alltoallw`
