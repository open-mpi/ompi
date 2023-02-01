.. _mpi_reduce_scatter:


MPI_Reduce_scatter
==================

.. include_body

:ref:`MPI_Reduce_scatter`, :ref:`MPI_Ireduce_scatter`, :ref:`MPI_Reduce_scatter_init` -
Combines values and scatters the results.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Reduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[],
   	MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)

   int MPI_Ireduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[],
   	MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request)

   int MPI_Reduce_scatter_init(const void *sendbuf, void *recvbuf,
        const int recvcounts[], MPI_Datatype datatype, MPI_Op op,
        MPI_Comm comm, MPI_Info info, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_REDUCE_SCATTER(SENDBUF, RECVBUF, RECVCOUNTS, DATATYPE, OP,
   		COMM, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	RECVCOUNTS(*), DATATYPE, OP, COMM, IERROR

   MPI_IREDUCE_SCATTER(SENDBUF, RECVBUF, RECVCOUNTS, DATATYPE, OP,
   		COMM, REQUEST, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	RECVCOUNTS(*), DATATYPE, OP, COMM, REQUEST, IERROR

   MPI_REDUCE_SCATTER_INIT(SENDBUF, RECVBUF, RECVCOUNTS, DATATYPE, OP,
   		COMM, INFO, REQUEST, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	RECVCOUNTS(*), DATATYPE, OP, COMM, INFO, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Reduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op, comm,
   		ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
   	TYPE(*), DIMENSION(..) :: recvbuf
   	INTEGER, INTENT(IN) :: recvcounts(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Op), INTENT(IN) :: op
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Ireduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op, comm,
   		request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Op), INTENT(IN) :: op
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Reduce_scatter_init(sendbuf, recvbuf, recvcounts, datatype, op, comm,
   		info, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Op), INTENT(IN) :: op
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``sendbuf``: Starting address of send buffer (choice).
* ``recvcounts``: Integer array specifying the number of elements in result distributed to each process. Array must be identical on all calling processes.
* ``datatype``: Datatype of elements of input buffer (handle).
* ``op``: Operation (handle).
* ``comm``: Communicator (handle).
* ``info``: Info (handle, persistent).

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Starting address of receive buffer (choice).
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Reduce_scatter` first does an element-wise reduction on vector of
*count* = S(i)\ *recvcounts*\ [i] elements in the send buffer defined by
*sendbuf*, *count*, and *datatype*. Next, the resulting vector of
results is split into n disjoint segments, where n is the number of
processes in the group. Segment i contains *recvcounts*\ [i] elements.
The ith segment is sent to process i and stored in the receive buffer
defined by *recvbuf*, *recvcounts*\ [i], and *datatype*.


USE OF IN-PLACE OPTION
----------------------

When the communicator is an intracommunicator, you can perform a
reduce-scatter operation in-place (the output buffer is used as the
input buffer). Use the variable MPI_IN_PLACE as the value of the
*sendbuf*. In this case, the input data is taken from the top of the
receive buffer. The area occupied by the input data may be either longer
or shorter than the data filled by the output data.


WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR
------------------------------------------

When the communicator is an inter-communicator, the reduce-scatter
operation occurs in two phases. First, the result of the reduction
performed on the data provided by the processes in the first group is
scattered among the processes in the second group. Then the reverse
occurs: the reduction performed on the data provided by the processes in
the second group is scattered among the processes in the first group.
For each group, all processes provide the same *recvcounts* argument,
and the sum of the *recvcounts* values should be the same for both
groups.


NOTES ON COLLECTIVE OPERATIONS
------------------------------

The reduction functions ( MPI_Op ) do not return an error value. As a
result, if the functions detect an error, all they can do is either call
:ref:`MPI_Abort` or silently skip the problem. Thus, if you change the error
handler from MPI_ERRORS_ARE_FATAL to something else, for example,
MPI_ERRORS_RETURN , then no error may be indicated.

The reason for this is the performance problems in ensuring that all
collective routines return the same error value.


ERRORS
------

.. include:: ./ERRORS.rst
