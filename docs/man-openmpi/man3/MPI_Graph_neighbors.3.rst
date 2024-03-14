.. _mpi_graph_neighbors:


MPI_Graph_neighbors
===================

.. include_body

:ref:`MPI_Graph_neighbors` |mdash| Returns the neighbors of a node associated
with a graph topology.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Graph_neighbors(MPI_Comm comm, int rank, int maxneighbors,
   	int neighbors[])


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_GRAPH_NEIGHBORS(COMM, RANK, MAXNEIGHBORS, NEIGHBORS, IERROR)
   	INTEGER	COMM, RANK, MAXNEIGHBORS, NEIGHBORS(*), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Graph_neighbors(comm, rank, maxneighbors, neighbors, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(IN) :: rank, maxneighbors
   	INTEGER, INTENT(OUT) :: neighbors(maxneighbors)
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``comm``: Communicator with graph topology (handle).
* ``rank``: Rank of process in group of comm (integer).
* ``maxneighbors``: Size of array neighbors (integer).

OUTPUT PARAMETERS
-----------------
* ``neighbors``: Ranks of processes that are neighbors to specified process (array of integers).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

**Example:** Suppose that comm is a communicator with a shuffle-exchange
topology. The group has 2n members. Each process is labeled by a(1),
..., a(n) with a(i) E{0,1}, and has three neighbors: exchange (a(1),
..., a(n) = a(1), ..., a(n-1), a(n) (a = 1 - a), shuffle (a(1), ...,
a(n)) = a(2), ..., a(n), a(1), and unshuffle (a(1), ..., a(n)) = a(n),
a(1), ..., a(n-1). The graph adjacency list is illustrated below for
n=3.

::

           		exchange		shuffle		unshuffle
       node		neighbors(1)	neighbors(2)	neighbors(3)
       0(000)	    1		    0		    0
       1(001)	    0		    2		    4
       2(010)	    3		    4		    1
       3(011)	    2		    6		    5
       4(100)	    5		    1		    2
       5(101)	    4		    3		    6
       6(110)	    7		    5		    3
       7(111)	    6		    7		    7

Suppose that the communicator comm has this topology associated with it.
The following code fragment cycles through the three types of neighbors
and performs an appropriate permutation for each.

.. code-block:: fortran

   !  assume: each process has stored a real number A.
   !  extract neighborhood information
   CALL MPI_COMM_RANK(comm, myrank, ierr)
   CALL MPI_GRAPH_NEIGHBORS(comm, myrank, 3, neighbors, ierr)
   !  perform exchange permutation
   CALL MPI_SENDRECV_REPLACE(A, 1, MPI_REAL, neighbors(1), 0, &
                             neighbors(1), 0, comm, status, ierr)
   !  perform shuffle permutation
   CALL MPI_SENDRECV_REPLACE(A, 1, MPI_REAL, neighbors(2), 0, &
                             neighbors(3), 0, comm, status, ierr)
   !  perform unshuffle permutation
   CALL MPI_SENDRECV_REPLACE(A, 1, MPI_REAL, neighbors(3), 0, &
                             neighbors(2), 0, comm, status, ierr)


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Graph_neighbors_count`
