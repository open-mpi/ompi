.. _mpi_dist_graph_create:


MPI_Dist_graph_create
=====================

.. include_body

:ref:`MPI_Dist_graph_create` |mdash| Makes a new communicator to which topology
information has been attached.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Dist_graph_create(MPI_Comm comm_old, int n, const int sources[],
   	const int degrees[], const int destinations[], const int weights[],
           MPI_Info info, int reorder, MPI_Comm *comm_dist_graph)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_DIST_GRAPH_CREATE(COMM_OLD, N, SOURCES, DEGREES, DESTINATIONS, WEIGHTS,
                   INFO, REORDER, COMM_DIST_GRAPH, IERROR)
   	INTEGER	COMM_OLD, N, SOURCES(*), DEGRES(*), WEIGHTS(*), INFO
   	INTEGER	COMM_DIST_GRAPH, IERROR
   	LOGICAL   REORDER


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Dist_Graph_create(comm_old, n, sources, degrees, destinations, weights,
   		info, reorder, comm_dist_graph, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm_old
   	INTEGER, INTENT(IN) :: n, sources(n), degrees(n), destinations(*)
   	INTEGER, INTENT(IN) :: weights(*)
   	TYPE(MPI_Info), INTENT(IN) :: info
   	LOGICAL, INTENT(IN) :: reorder
   	TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``comm_old``: Input communicator without topology (handle).
* ``n``: Number of source nodes for which this process specifies edges (non-negative integer).
* ``sources``: Array containing the *n* source nodes for which this process specifies edges (array of non-negative integers).
* ``degrees``: Array specifying the number of destinations for each source node in the source node array (array of non-negative integers).
* ``destinations``: Destination nodes for the source nodes in the source node array (array of non-negative integers).
* ``weights``: Weights for source to destination edges (array of non-negative integers).
* ``info``: Hints on optimization and interpretation of weights (handle).
* ``reorder``: Ranking may be reordered (true) or not (false) (logical).

OUTPUT PARAMETERS
-----------------
* ``comm_dist_graph``: Communicator with distributed graph topology added (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Dist_graph_create` creates a new communicator *comm_dist_graph* with
distributed graph topology and returns a handle to the new communicator.
The number of processes in *comm_dist_graph* is identical to the number
of processes in *comm_old*. Concretely, each process calls the
constructor with a set of directed (source,destination) communication
edges as described below. Every process passes an array of *n* source
nodes in the *sources* array. For each source node, a non-negative
number of destination nodes is specified in the *degrees* array. The
destination nodes are stored in the corresponding consecutive segment of
the *destinations* array. More precisely, if the i-th node in sources is
s, this specifies *degrees*\ [i] *edges* (s,d) with d of the j-th such
edge stored in
*destinations*\ [*degrees*\ [0]+...+\ *degrees*\ [i-1]+j]. The weight of
this edge is stored in
*weights*\ [*degrees*\ [0]+...+\ *degrees*\ [i-1]+j]. Both the *sources*
and the *destinations* arrays may contain the same node more than once,
and the order in which nodes are listed as destinations or sources is
not signicant. Similarly, different processes may specify edges with the
same source and destination nodes. Source and destination nodes must be
process ranks of comm_old. Different processes may specify different
numbers of source and destination nodes, as well as different source to
destination edges. This allows a fully distributed specification of the
communication graph. Isolated processes (i.e., processes with no
outgoing or incoming edges, that is, processes that do not occur as
source or destination node in the graph specification) are allowed. The
call to :ref:`MPI_Dist_graph_create` is collective.

If reorder = false, all processes will have the same rank in
comm_dist_graph as in comm_old. If reorder = true then the MPI library
is free to remap to other processes (of comm_old) in order to improve
communication on the edges of the communication graph. The weight
associated with each edge is a hint to the MPI library about the amount
or intensity of communication on that edge, and may be used to compute a


WEIGHTS
-------

Weights are specified as non-negative integers and can be used to
influence the process remapping strategy and other internal MPI
optimizations. For instance, approximate count arguments of later
communication calls along specific edges could be used as their edge
weights. Multiplicity of edges can likewise indicate more intense
communication between pairs of processes. However, the exact meaning of
edge weights is not specified by the MPI standard and is left to the
implementation. An application can supply the special value
MPI_UNWEIGHTED for the weight array to indicate that all edges have the
same (effectively no) weight. It is erroneous to supply MPI_UNWEIGHTED
for some but not all processes of comm_old. If the graph is weighted but
*n* = 0, then MPI_WEIGHTS_EMPTY or any arbitrary array may be passed to
weights. Note that MPI_UNWEIGHTED and MPI_WEIGHTS_EMPTY are not special
weight values; rather they are special values for the total array
argument. In Fortran, MPI_UNWEIGHTED and MPI_WEIGHTS_EMPTY are objects
like MPI_BOTTOM (not usable for initialization or assignment). See MPI-3
section 2.5.4.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Dist_graph_create_adjacent`
   * :ref:`MPI_Dist_graph_neighbors`
   * :ref:`MPI_Dist_graph_neighbors_count`
