.. _mpi_gatherv:

MPI_Gatherv
===========

.. include_body

:ref:`MPI_Gatherv`, :ref:`MPI_Igatherv`, :ref:`MPI_Gatherv_init` - Gathers varying amounts of
data from all processes to the root process

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
       void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype recvtype,
       int root, MPI_Comm comm)

   int MPI_Igatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
       void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype recvtype,
       int root, MPI_Comm comm, MPI_Request *request)

   int MPI_Gatherv_init(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
       void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype recvtype,
       int root, MPI_Comm comm, MPI_Info info, MPI_Request *request)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNTS,
           DISPLS, RECVTYPE, ROOT, COMM, IERROR)
       <type>  SENDBUF(*), RECVBUF(*)
       INTEGER SENDCOUNT, SENDTYPE, RECVCOUNTS(*), DISPLS(*)
       INTEGER RECVTYPE, ROOT, COMM, IERROR

   MPI_IGATHERV(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNTS,
           DISPLS, RECVTYPE, ROOT, COMM, REQUEST, IERROR)
       <type>  SENDBUF(*), RECVBUF(*)
       INTEGER SENDCOUNT, SENDTYPE, RECVCOUNTS(*), DISPLS(*)
       INTEGER RECVTYPE, ROOT, COMM, REQUEST, IERROR

   MPI_GATHERV_INIT(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNTS,
           DISPLS, RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR)
       <type>  SENDBUF(*), RECVBUF(*)
       INTEGER SENDCOUNT, SENDTYPE, RECVCOUNTS(*), DISPLS(*)
       INTEGER RECVTYPE, ROOT, COMM, INFO, REQUEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Gatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs,
           recvtype, root, comm, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
       TYPE(*), DIMENSION(..) :: recvbuf
       INTEGER, INTENT(IN) :: sendcount, recvcounts(*), displs(*), root
       TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Igatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs,
           recvtype, root, comm, request, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
       INTEGER, INTENT(IN) :: sendcount, root
       INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*), displs(*)
       TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
       TYPE(MPI_Comm), INTENT(IN) :: comm
       TYPE(MPI_Request), INTENT(OUT) :: request
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Gatherv_init(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs,
           recvtype, root, comm, info, request, ierror)
       TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
       INTEGER, INTENT(IN) :: sendcount, root
       INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*), displs(*)
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
* ``recvcounts`` : Integer array (of length group size) containing the
   number of elements that are received from each process (significant
   only at root).
* ``displs`` : Integer array (of length group size). Entry i specifies the
   displacement relative to recvbuf at which to place the incoming data
   from process i (significant only at root).
* ``recvtype`` : Datatype of recv buffer elements (significant only at
   root) (handle).
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

:ref:`MPI_Gatherv` extends the functionality of :ref:`MPI_Gather` by allowing a
varying count of data from each process, since recvcounts is now an
array. It also allows more flexibility as to where the data is placed on
the root, by providing the new argument, displs.

The outcome is as if each process, including the root process, sends a
message to the root,

.. code-block:: c
   
   MPI_Send(sendbuf, sendcount, sendtype, root, ...);

and the root executes n receives,

.. code-block:: c
   
   MPI_Recv(recvbuf + disp[i] * extent(recvtype), recvcounts[i],
            recvtype, i, ...);

Messages are placed in the receive buffer of the root process in rank
order, that is, the data sent from process j is placed in the jth
portion of the receive buffer recvbuf on process root. The jth portion
of recvbuf begins at offset displs[j] elements (in terms of recvtype)
into recvbuf.

The receive buffer is ignored for all nonroot processes.

The type signature implied by sendcount, sendtype on process i must be
equal to the type signature implied by recvcounts[i], recvtype at the
root. This implies that the amount of data sent must be equal to the
amount of data received, pairwise between each process and the root.
Distinct type maps between sender and receiver are still allowed, as
illustrated in Example 2, below.

All arguments to the function are significant on process root, while on
other processes, only arguments sendbuf, sendcount, sendtype, root, comm
are significant. The arguments root and comm must have identical values
on all processes.

The specification of counts, types, and displacements should not cause
any location on the root to be written more than once. Such a call is
erroneous.

Example 1: Now have each process send 100 ints to root, but place each
set (of 100) stride ints apart at receiving end. Use :ref:`MPI_Gatherv` and the
displs argument to achieve this effect. Assume stride >= 100.

.. code-block:: c

   MPI_Comm comm;
   int gsize, sendarray[100];
   int root, *rbuf, stride;
   int *displs, i, rcounts;
   ...

   MPI_Comm_size(comm, &gsize);
   rbuf = (int)malloc(gsize * stride * sizeof(int));
   displs = (int)malloc(gsize * sizeof(int));
   rcounts = (int )malloc(gsize * sizeof(int));

   for (i=0; i<gsize; ++i) {
     displs[i] = i * stride;
     rcounts[i] = 100;
   }
   MPI_Gatherv(sendarray, 100, MPI_INT, rbuf, rcounts, displs, MPI_INT,
               root, comm);

Note that the program is erroneous if stride < 100.

Example 2: Same as Example 1 on the receiving side, but send the 100
ints from the 0th column of a 100 150 int array, in C.

.. code-block:: c
   
   MPI_Comm comm;
   int gsize, sendarray[100][150];
   int root, *rbuf, stride;
   MPI_Datatype stype;
   int displs,i, rcounts;
   ...

   MPI_Comm_size(comm, &gsize);
   rbuf = (int )malloc(gsize * stride * sizeof(int));
   displs = (int)malloc(gsize * sizeof(int));
   rcounts = (int )malloc(gsize * sizeof(int));

   for (i=0; i<gsize; ++i) {
     displs[i] = i * stride;
     rcounts[i] = 100;
   }

   // Create datatype for 1 column of array
   MPI_Type_vector(100, 1, 150, MPI_INT, &stype);
   MPI_Type_commit( &stype );
   MPI_Gatherv(sendarray, 1, stype, rbuf, rcounts, displs, MPI_INT,
               root, comm);

Example 3: Process i sends (100-i) ints from the ith column of a 100 x
150 int array, in C. It is received into a buffer with stride, as in the
previous two examples.

.. code-block:: c
   
   MPI_Comm comm;
   int gsize, sendarray[100][150], *sptr;
   int root, *rbuf, stride, myrank;
   MPI_Datatype stype;
   int displs, i, rcounts;
   ...
   
   MPI_Comm_size(comm, &gsize);
   MPI_Comm_rank( comm, &myrank );
   rbuf = (int)malloc(gsize * stride * sizeof(int));
   displs = (int)malloc(gsize * sizeof(int));
   rcounts = (int )malloc(gsize * sizeof(int));
   
   for (i=0; i<gsize; ++i) {
     displs[i] = i * stride;
     rcounts[i] = 100-i; // note change from previous example
   }

   // Create datatype for the column we are sending
   MPI_Type_vector(100-myrank, 1, 150, MPI_INT, &stype);
   MPI_Type_commit( &stype );
   // sptr is the address of start of "myrank" column
   sptr = &sendarray[0][myrank];
   MPI_Gatherv(sptr, 1, stype, rbuf, rcounts, displs, MPI_INT,
               root, comm);

Note that a different amount of data is received from each process.

Example 4: Same as Example 3, but done in a different way at the sending
end. We create a datatype that causes the correct striding at the
sending end so that we read a column of a C array.

.. code-block:: c
   
   MPI_Comm comm;
   int gsize, sendarray[100][150], *sptr;
   int root, *rbuf, stride, myrank, disp[2], blocklen[2];
   MPI_Datatype stype, type[2];
   int displs, i, rcounts;
   ...

   MPI_Comm_size(comm, &gsize);
   MPI_Comm_rank(comm, &myrank );
   rbuf = (int )alloc(gsize * stride * sizeof(int));
   displs = (int )malloc(gsize * sizeof(int));
   rcounts = (int)malloc(gsize * sizeof(int));

   for (i=0; i<gsize; ++i) {
     displs[i] = i* stride;
     rcounts[i] = 100-i;
   }
   // Create datatype for one int, with extent of entire row
   disp[0] = 0;
   disp[1] = 150 * sizeof(int);
   type[0] = MPI_INT;
   type[1] = MPI_UB;
   blocklen[0] = 1;
   blocklen[1] = 1;

   MPI_Type_struct( 2, blocklen, disp, type, &stype );
   MPI_Type_commit(&stype );
   sptr = &sendarray[0][myrank];
   MPI_Gatherv(sptr, 100-myrank, stype, rbuf, rcounts, displs, MPI_INT,
               root, comm);

Example 5: Same as Example 3 at sending side, but at receiving side we
make the stride between received blocks vary from block to block.

.. code-block:: c
   
   MPI_Comm comm;
   int gsize, sendarray[100][150], *sptr;
   int root, *rbuf, *stride, myrank, bufsize;
   MPI_Datatype stype;
   int *displs, i, *rcounts, offset;
   ...

   MPI_Comm_size( comm, &gsize);
   MPI_Comm_rank( comm, &myrank );
   de = (int )malloc(gsize * sizeof(int));
   ...
   // stride[i] for i = 0 to gsize-1 is set somehow

   // set up displs and rcounts vectors first
   displs = (int)malloc(gsize * sizeof(int));
   rcounts = (int )malloc(gsize * sizeof(int));
   offset = 0;

   for (i=0; i<gsize; ++i) {
     displs[i] = offset;
     offset += stride[i];
     rcounts[i] = 100-i;
   }

   // the required buffer size for rbuf is now easily obtained
   bufsize = displs[gsize-1]+rcounts[gsize-1];
   rbuf = (int )malloc(bufsize * sizeof(int));
   // Create datatype for the column we are sending
   MPI_Type_vector(100-myrank, 1, 150, MPI_INT, &stype);
   MPI_Type_commit( &stype );
   sptr = &sendarray[0][myrank];
   MPI_Gatherv(sptr, 1, stype, rbuf, rcounts, displs, MPI_INT,
               root, comm);

Example 6: Process i sends num ints from the ith column of a 100 x 150
int array, in C. The complicating factor is that the various values of
num are not known to root, so a separate gather must first be run to
find these out. The data is placed contiguously at the receiving end.

.. code-block:: c

   MPI_Comm comm;
   int gsize, sendarray[100][150], *sptr;
   int root, *rbuf, stride, myrank, disp[2], blocklen[2];
   MPI_Datatype stype,types[2];
   int *displs, i, *rcounts, num;
   ...

   MPI_Comm_size( comm, &gsize);
   MPI_Comm_rank( comm, &myrank );

   // First, gather nums to root
   rcounts = (int )malloc(gsize * sizeof(int));
   MPI_Gather( &num, 1, MPI_INT, rcounts, 1, MPI_INT, root, comm);
   // root now has correct rcounts, using these we set
   // displs[] so that data is placed contiguously (or concatenated) at receive end

   displs = (int)malloc(gsize * sizeof(int));
   displs[0] = 0;
   for (i=1; i<gsize; ++i) {
     displs[i] = displs[i-1]+rcounts[i-1];
   }

   // And, create receive buffer
   rbuf = (int *)malloc(gsize * (displs[gsize-1]+rcounts[gsize-1]) * sizeof(int));
   // Create datatype for one int, with extent of entire row
   disp[0] = 0;
   disp[1] = 150 * sizeof(int);
   type[0] = MPI_INT;
   type[1] = MPI_UB;
   blocklen[0] = 1;
   blocklen[1] = 1;
   MPI_Type_struct(2, blocklen, disp, type, &stype );
   MPI_Type_commit( &stype );
   sptr = &sendarray[0][myrank];
   MPI_Gatherv(sptr, num, stype, rbuf, rcounts, displs, MPI_INT, root, comm);


Use Of In-Place Option
----------------------

The in-place option operates in the same way as it does for :ref:`MPI_Gather`.
When the communicator is an intracommunicator, you can perform a gather
operation in-place (the output buffer is used as the input buffer). Use
the variable MPI_IN_PLACE as the value of the root process sendbuf. In
this case, sendcount and sendtype are ignored, and the contribution of
the root process to the gathered vector is assumed to already be in the
correct place in the receive buffer.

Note that MPI_IN_PLACE is a special kind of value; it has the same
restrictions on its use as MPI_BOTTOM.

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

.. seealso:: :ref:`MPI_Gather`
