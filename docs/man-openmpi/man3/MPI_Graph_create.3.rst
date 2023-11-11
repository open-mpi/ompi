.. _mpi_graph_create:

MPI_Graph_create
================

.. include_body

:ref:`MPI_Graph_create` |mdash| Makes a new communicator to which topology
information has been attached.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Graph_create(MPI_Comm comm_old, int nnodes, const int index[],
       const int edges[], int reorder, MPI_Comm *comm_graph)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GRAPH_CREATE(COMM_OLD, NNODES, INDEX, EDGES, REORDER,
           COMM_GRAPH, IERROR)
       INTEGER COMM_OLD, NNODES, INDEX(*), EDGES(*)
       INTEGER COMM_GRAPH, IERROR
       LOGICAL   REORDER

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Graph_create(comm_old, nnodes, index, edges, reorder, comm_graph,
           ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm_old
       INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
       LOGICAL, INTENT(IN) :: reorder
       TYPE(MPI_Comm), INTENT(OUT) :: comm_graph
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``comm_old`` : Input communicator without topology (handle).
* ``nnodes`` : Number of nodes in graph (integer).
* ``index`` : Array of integers describing node degrees (see below).
* ``edges`` : Array of integers describing graph edges (see below).
* ``reorder`` : Ranking may be reordered (true) or not (false) (logical).

OUTPUT PARAMETERS
-----------------

* ``comm_graph`` : Communicator with graph topology added (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Graph_create` returns a handle to a new communicator to which the
graph topology information is attached. If reorder = false then the rank
of each process in the new group is identical to its rank in the old
group. Otherwise, the function may reorder the processes. If the size,
nnodes, of the graph is smaller than the size of the group of comm_old,
then some processes are returned MPI_COMM_NULL, in analogy to
:ref:`MPI_Cart_create` and :ref:`MPI_Comm_split`. The call is erroneous if it
specifies a graph that is larger than the group size of the input
communicator.

The three parameters nnodes, index, and edges define the graph
structure. nnodes is the number of nodes of the graph. The nodes are
numbered from 0 to nnodes-1. The ith entry of array index stores the
total number of neighbors of the first i graph nodes. The lists of
neighbors of nodes 0, 1, ..., nnodes-1 are stored in consecutive
locations in array edges. The array edges is a flattened representation
of the edge lists. The total number of entries in index is nnodes and
the total number of entries in edges is equal to the number of graph
edges.

The definitions of the arguments nnodes, index, and edges are
illustrated with the following simple example.

Example: Assume there are four processes 0, 1, 2, 3 with the following
adjacency matrix:

------- ---------
Process Neighbors
------- ---------
0       1, 3
1       0
2       3
3       0, 2
------- ---------

Then, the input arguments are:

* nodes = 4
* index = 2, 3, 4, 6
* edges = 1, 3, 0, 3, 0, 2

Thus, in C, index[0] is the degree of node zero, and index[i] -
index[i-1] is the degree of node i, i=1, . . . , nnodes-1; the list of
neighbors of node zero is stored in edges[j], for 0 <= j <= index[0] - 1
and the list of neighbors of node i, i > 0 , is stored in edges[j],
index[i-1] <= j <= index[i] - 1.

In Fortran, index(1) is the degree of node zero, and index(i+1) -
index(i) is the degree of node i, i=1, . . . , nnodes-1; the list of
neighbors of node zero is stored in edges(j), for 1 <= j <= index(1) and
the list of neighbors of node i, i > 0, is stored in edges(j), index(i)
+ 1 <= j <= index(i + 1).

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Graph_get`
