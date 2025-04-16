.. _mpi_alltoallw:


MPI_Alltoallw
=============

.. include_body

:ref:`MPI_Alltoallw`, :ref:`MPI_Ialltoallw`, :ref:`MPI_Alltoallw_init` - All processes
send data of different types to, and receive data of different types
from, all processes

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Alltoallw, MPI_Ialltoallw, MPI_Alltoallw_init

.. The following file was automatically generated
.. include:: ./bindings/mpi_alltoallw.rst

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
* ``request``: Request (handle, non-blocking and persistent only).
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
