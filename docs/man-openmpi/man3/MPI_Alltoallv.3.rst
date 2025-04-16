.. _mpi_alltoallv:


MPI_Alltoallv
=============

.. include_body

:ref:`MPI_Alltoallv`, :ref:`MPI_Ialltoallv`, :ref:`MPI_Alltoallv_init` - All processes
send different amount of data to, and receive different amount of data
from, all processes

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Alltoallv, MPI_Ialltoallv, MPI_Alltoallv_init

.. The following file was automatically generated
.. include:: ./bindings/mpi_alltoallv.rst

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
* ``request``: Request (handle, non-blocking and persistent only).
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
