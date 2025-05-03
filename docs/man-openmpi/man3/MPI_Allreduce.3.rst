.. _mpi_allreduce:


MPI_Allreduce
=============

.. include_body

:ref:`MPI_Allreduce`, :ref:`MPI_Iallreduce`, :ref:`MPI_Allreduce_init` - Combines values
from all processes and distributes the result back to all processes.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Allreduce, MPI_Iallreduce, MPI_Allreduce_init

.. The following file was automatically generated
.. include:: ./bindings/mpi_allreduce.rst

INPUT PARAMETERS
----------------
* ``sendbuf``: Starting address of send buffer (choice).
* ``count``: Number of elements in send buffer (integer).
* ``datatype``: Datatype of elements of send buffer (handle).
* ``op``: Operation (handle).
* ``comm``: Communicator (handle).
* ``info``: Info (handle, persistent only).

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Starting address of receive buffer (choice).
* ``request``: Request (handle, non-blocking and persistent only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Same as :ref:`MPI_Reduce` except that the result appears in the receive buffer
of all the group members.

**Example 1:** A routine that computes the product of a vector and an
array that are distributed across a group of processes and returns the
answer at all nodes (compare with Example 2, with :ref:`MPI_Reduce`, below).

.. code-block:: fortran

   SUBROUTINE PAR_BLAS2(m, n, a, b, c, comm)
   REAL a(m), b(m,n)    ! local slice of array
   REAL c(n)            ! result
   REAL sum(n)
   INTEGER n, comm, i, j, ierr

   ! local sum
   DO j= 1, n
     sum(j) = 0.0
     DO i = 1, m
       sum(j) = sum(j) + a(i)*b(i,j)
     END DO
   END DO

   ! global sum
   CALL MPI_ALLREDUCE(sum, c, n, MPI_REAL, MPI_SUM, comm, ierr)

   ! return result at all nodes
   RETURN

**Example 2:** A routine that computes the product of a vector and an
array that are distributed across a group of processes and returns the
answer at node zero.

.. code-block:: fortran

   SUBROUTINE PAR_BLAS2(m, n, a, b, c, comm)
   REAL a(m), b(m,n)    ! local slice of array
   REAL c(n)            ! result
   REAL sum(n)
   INTEGER n, comm, i, j, ierr

   ! local sum
   DO j= 1, n
     sum(j) = 0.0
     DO i = 1, m
       sum(j) = sum(j) + a(i)*b(i,j)
     END DO
   END DO

   ! global sum
   CALL MPI_REDUCE(sum, c, n, MPI_REAL, MPI_SUM, 0, comm, ierr)

   ! return result at node zero (and garbage at the other nodes)
   RETURN


USE OF IN-PLACE OPTION
----------------------

When the communicator is an intracommunicator, you can perform an
all-reduce operation in-place (the output buffer is used as the input
buffer). Use the variable MPI_IN_PLACE as the value of *sendbuf* at all
processes.

Note that MPI_IN_PLACE is a special kind of value; it has the same
restrictions on its use as MPI_BOTTOM.

Because the in-place option converts the receive buffer into a
send-and-receive buffer, a Fortran binding that includes INTENT must
mark these as INOUT, not OUT.


WHEN COMMUNICATOR IS AN INTER-COMMUNICATOR
------------------------------------------

When the communicator is an inter-communicator, the reduce operation
occurs in two phases. The data is reduced from all the members of the
first group and received by all the members of the second group. Then
the data is reduced from all the members of the second group and
received by all the members of the first. The operation exhibits a
symmetric, full-duplex behavior.

When the communicator is an intra-communicator, these groups are the
same, and the operation occurs in a single phase.


NOTES ON COLLECTIVE OPERATIONS
------------------------------

The reduction functions ( MPI_Op ) do not return an error value. As a
result, if the functions detect an error, all they can do is either call
:ref:`MPI_Abort` or silently skip the problem. Thus, if you change the error
handler from MPI_ERRORS_ARE_FATAL to something else, for example,
MPI_ERRORS_RETURN , then no error may be indicated.


ERRORS
------

.. include:: ./ERRORS.rst
