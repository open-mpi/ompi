.. _mpi_scatterv:


MPI_Scatterv
============

.. include_body

:ref:`MPI_Scatterv`, :ref:`MPI_Iscatterv`, :ref:`MPI_Scatterv_init` - Scatters a buffer
in parts to all tasks in a group.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Scatterv(const void *sendbuf, const int sendcounts[], const int displs[],
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	MPI_Datatype recvtype, int root, MPI_Comm comm)

   int MPI_Iscatterv(const void *sendbuf, const int sendcounts[], const int displs[],
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request)

   int MPI_Scatterv_init(const void *sendbuf, const int sendcounts[], const int displs[],
   	MPI_Datatype sendtype, void *recvbuf, int recvcount,
   	MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SCATTERV(SENDBUF, SENDCOUNTS, DISPLS, SENDTYPE, RECVBUF,
   		RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), DISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR

   MPI_ISCATTERV(SENDBUF, SENDCOUNTS, DISPLS, SENDTYPE, RECVBUF,
   		RECVCOUNT, RECVTYPE, ROOT, COMM, REQUEST, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), DISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNT, RECVTYPE, ROOT, COMM, REQUEST, IERROR

   MPI_SCATTERV_INIT(SENDBUF, SENDCOUNTS, DISPLS, SENDTYPE, RECVBUF,
   		RECVCOUNT, RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	SENDCOUNTS(*), DISPLS(*), SENDTYPE
   	INTEGER	RECVCOUNT, RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Scatterv(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount,
   		recvtype, root, comm, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
   	TYPE(*), DIMENSION(..) :: recvbuf
   	INTEGER, INTENT(IN) :: sendcounts(*), displs(*), recvcount, root
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Iscatterv(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount,
   		recvtype, root, comm, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), displs(*)
   	INTEGER, INTENT(IN) :: recvcount, root
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_scatterv_init(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount,
   		recvtype, root, comm, info, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), displs(*)
   	INTEGER, INTENT(IN) :: recvcount, root
   	TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``sendbuf``: Address of send buffer (choice, significant only at root).
* ``sendcounts``: Integer array (of length group size) specifying the number of elements to send to each processor.
* ``displs``: Integer array (of length group size). Entry i specifies the displacement (relative to sendbuf) from which to take the outgoing data to process i.
* ``sendtype``: Datatype of send buffer elements (handle).
* ``recvcount``: Number of elements in receive buffer (integer).
* ``recvtype``: Datatype of receive buffer elements (handle).
* ``root``: Rank of sending process (integer).
* ``comm``: Communicator (handle).
* ``info``: Info (handle, persistent only).

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Address of receive buffer (choice).
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Scatterv` is the inverse operation to :ref:`MPI_Gatherv`.

:ref:`MPI_Scatterv` extends the functionality of :ref:`MPI_Scatter` by allowing a
varying count of data to be sent to each process, since *sendcounts* is
now an array. It also allows more flexibility as to where the data is
taken from on the root, by providing the new argument, *displs*.

The outcome is as if the root executed *n* send operations,

.. code-block:: c

       MPI_Send(sendbuf + displs[i] * extent(sendtype),
                sendcounts[i], sendtype, i, ...);

       // and each process executed a receive,

       MPI_Recv(recvbuf, recvcount, recvtype, root, ...)

The send buffer is ignored for all nonroot processes.

The type signature implied by *sendcount*\ [*i*], *sendtype* at the root
must be equal to the type signature implied by *recvcount*, *recvtype*
at process *i* (however, the type maps may be different). This implies
that the amount of data sent must be equal to the amount of data
received, pairwise between each process and the root. Distinct type maps
between sender and receiver are still allowed.

All arguments to the function are significant on process *root*, while
on other processes, only arguments *recvbuf*, *recvcount*, *recvtype*,
*root*, *comm* are significant. The arguments *root* and *comm* must
have identical values on all processes.

The specification of counts, types, and displacements should not cause
any location on the root to be read more than once.

**Example 1:** The reverse of Example 5 in the :ref:`MPI_Gatherv` manpage. We
have a varying stride between blocks at sending (root) side, at the
receiving side we receive 100 - *i* elements into the *i*\ th column of
a 100 x 150 C array at process *i*.

.. code-block:: c

           MPI_Comm comm;
           int gsize,recvarray[100][150],*rptr;
           int root, *sendbuf, myrank, bufsize, *stride;
           MPI_Datatype rtype;
           int i, *displs, *scounts, offset;
           ...
           MPI_Comm_size( comm, &gsize);
           MPI_Comm_rank( comm, &myrank );

           stride = (int *)malloc(gsize*sizeof(int));
           ...
           /* stride[i] for i = 0 to gsize-1 is set somehow
            * sendbuf comes from elsewhere
            */
           ...
           displs = (int *)malloc(gsize*sizeof(int));
           scounts = (int *)malloc(gsize*sizeof(int));
           offset = 0;
           for (i=0; i<gsize; ++i) {
               displs[i] = offset;
               offset += stride[i];
               scounts[i] = 100 - i;
           }
           /* Create datatype for the column we are receiving
            */
           MPI_Type_vector( 100-myrank, 1, 150, MPI_INT, &rtype);
           MPI_Type_commit( &rtype );
           rptr = &recvarray[0][myrank];
           MPI_Scatterv(sendbuf, scounts, displs, MPI_INT,
                        rptr, 1, rtype, root, comm);

**Example 2:** The reverse of Example 1 in the MPI_Gather manpage. The
root process scatters sets of 100 ints to the other processes, but the
sets of 100 are stride ints apart in the sending buffer. Requires use of
:ref:`MPI_Scatterv`, where *stride* >= 100.

.. code-block:: c

       MPI_Comm comm;
       int gsize,*sendbuf;
       int root, rbuf[100], i, *displs, *scounts;

       ...

       MPI_Comm_size(comm, &gsize);
       sendbuf = (int *)malloc(gsize*stride*sizeof(int));
       ...
       displs = (int *)malloc(gsize*sizeof(int));
       scounts = (int *)malloc(gsize*sizeof(int));
       for (i=0; i<gsize; ++i) {
          displs[i] = i*stride;
          scounts[i] = 100;
       }
       MPI_Scatterv(sendbuf, scounts, displs, MPI_INT,
                    rbuf, 100, MPI_INT, root, comm);


USE OF IN-PLACE OPTION
----------------------

When the communicator is an intracommunicator, you can perform a scatter
operation in-place (the output buffer is used as the input buffer). Use
the variable MPI_IN_PLACE as the value of the root process *recvbuf*. In
this case, *recvcount* and *recvtype* are ignored, and the root process
sends no data to itself.

Note that MPI_IN_PLACE is a special kind of value; it has the same
restrictions on its use as MPI_BOTTOM.

Because the in-place option converts the receive buffer into a
send-and-receive buffer, a Fortran binding that includes INTENT must
mark these as INOUT, not OUT.


WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR
------------------------------------------

When the communicator is an inter-communicator, the root process in the
first group sends data to all processes in the second group. The first
group defines the root process. That process uses MPI_ROOT as the value
of its *root* argument. The remaining processes use ``MPI_PROC_NULL`` as the
value of their *root* argument. All processes in the second group use
the rank of that root process in the first group as the value of their
*root* argument. The receive buffer argument of the root process in the
first group must be consistent with the receive buffer argument of the
processes in the second group.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Gather`
   * :ref:`MPI_Gatherv`
   * :ref:`MPI_Scatter`
