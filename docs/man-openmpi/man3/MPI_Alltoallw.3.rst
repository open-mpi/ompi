.. _mpi_alltoallw:


MPI_Alltoallw
=============

.. include_body

:ref:`MPI_Alltoallw`, :ref:`MPI_Ialltoallw`, :ref:`MPI_Alltoallw_init` - All processes
send data of different types to, and receive data of different types
from, all processes


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Alltoallw(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], const MPI_Datatype sendtypes[],
   	void *recvbuf, const int recvcounts[], const int rdispls[],
   	const MPI_Datatype recvtypes[], MPI_Comm comm)

   int MPI_Ialltoallw(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], const MPI_Datatype sendtypes[],
   	void *recvbuf, const int recvcounts[], const int rdispls[],
   	const MPI_Datatype recvtypes[], MPI_Comm comm,
   	MPI_Request *request)

   int MPI_Alltoallw_init(const void *sendbuf, const int sendcounts[],
   	const int sdispls[], const MPI_Datatype sendtypes[],
   	void *recvbuf, const int recvcounts[], const int rdispls[],
   	const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Info info,
   	MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_ALLTOALLW(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPES(*)
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPES(*)
   	INTEGER	COMM, IERROR

   MPI_IALLTOALLW(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, REQUEST, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPES(*)
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPES(*)
   	INTEGER	COMM, REQUEST, IERROR

   MPI_ALLTOALLW_INIT(SENDBUF, SENDCOUNTS, SDISPLS, SENDTYPES,
   	RECVBUF, RECVCOUNTS, RDISPLS, RECVTYPES, COMM, INFO, REQUEST, IERROR)

   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), SDISPLS(*), SENDTYPES(*)
   	INTEGER	RECVCOUNTS(*), RDISPLS(*), RECVTYPES(*)
   	INTEGER	COMM, INFO, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts,
   		rdispls, recvtypes, comm, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
   	TYPE(*), DIMENSION(..) :: recvbuf
   	INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*),
   	rdispls(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: recvtypes(*)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Ialltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf,
   		recvcounts, rdispls, recvtypes, comm, request, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*),
   	recvcounts(*), rdispls(*)
   	TYPE(MPI_Datatype), INTENT(IN), ASYNCHRONOUS :: sendtypes(*),
   	recvtypes(*)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Alltoallw_init(sendbuf, sendcounts, sdispls, sendtypes, recvbuf,
   		recvcounts, rdispls, recvtypes, comm, fIinfo, request, ierror)

   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*),
   	recvcounts(*), rdispls(*)
   	TYPE(MPI_Datatype), INTENT(IN), ASYNCHRONOUS :: sendtypes(*),
   	recvtypes(*)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``sendbuf``: Starting address of send buffer.
* ``sendcounts``: Integer array, where entry i specifies the number of elements to send to rank i.
* ``sdispls``: Integer array, where entry i specifies the displacement (in bytes, offset from *sendbuf) from which to send data to rank i.*
* ``sendtypes``: Datatype array, where entry i specifies the datatype to use when sending data to rank i.
* ``recvcounts``: Integer array, where entry j specifies the number of elements to receive from rank j.
* ``rdispls``: Integer array, where entry j specifies the displacement (in bytes, offset from *recvbuf) to which data from rank j should* be written.
* ``recvtypes``: Datatype array, where entry j specifies the datatype to use when receiving data from rank j.
* ``comm``: Communicator over which data is to be exchanged.
* ``info``: Info (handle, persistent only)

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Address of receive buffer.
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status.

DESCRIPTION
-----------

:ref:`MPI_Alltoallw` is a generalized collective operation in which all
processes send data to and receive data from all other processes. It
adds flexibility to :ref:`MPI_Alltoallv` by allowing the user to specify the
datatype of individual data blocks (in addition to displacement and
element count). Its operation can be thought of in the following way,
where each process performs 2n (n being the number of processes in
communicator *comm) independent point-to-point communications*
(including communication with itself).

.. code-block:: c

   	MPI_Comm_size(comm, &n);
   	for (i = 0, i < n; i++)
   	    MPI_Send(sendbuf + sdispls[i], sendcounts[i],
   	        sendtypes[i], i, ..., comm);
   	for (i = 0, i < n; i++)
   	    MPI_Recv(recvbuf + rdispls[i], recvcounts[i],
   	        recvtypes[i], i, ..., comm);

Process j sends the k-th block of its local *sendbuf to process* k,
which places the data in the j-th block of its local *recvbuf.*

When a pair of processes exchanges data, each may pass different element
count and datatype arguments so long as the sender specifies the same
amount of data to send (in bytes) as the receiver expects to receive.

Note that process i may send a different amount of data to process j
than it receives from process j. Also, a process may send entirely
different amounts and types of data to different processes in the
communicator.

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
this case, *sendcounts*, *sdispls*, and *sendtypes* are ignored. The
input data of each process is assumed to be in the area where that
process would receive its own contribution to the receive buffer.


NOTES
-----

The specification of counts, types, and displacements should not cause
any location to be written more than once.

All arguments on all processes are significant. The *comm* argument, in
particular, must describe the same communicator on all processes.

The offsets of *sdispls* and *rdispls* are measured in bytes. Compare
this to :ref:`MPI_Alltoallv`, where these offsets are measured in units of
*sendtype* and *recvtype*, respectively.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Alltoall`
   * :ref:`MPI_Alltoallv`
