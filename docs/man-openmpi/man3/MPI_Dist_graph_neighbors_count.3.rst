.. _mpi_dist_graph_neighbors_count:


MPI_Dist_graph_neighbors_count
==============================

.. include_body

:ref:`MPI_Dist_graph_neighbors_count` |mdash| Returns the number of in and out
edges for the calling processes in a distributed graph topology and a
flag indicating whether the distributed graph is weighted.

.. The following file was automatically generated
.. include:: ./bindings/mpi_dist_graph_neighbors_count.rst

INPUT PARAMETERS
----------------
* ``comm``: Communicator with distributed graph topology (handle).

OUTPUT PARAMETERS
-----------------
* ``indegree``: Number of edges into this process (non-negative integer).
* ``outdegree``: Number of edges out of this process (non-negative integer).
* ``weighted``: False if MPI_UNWEIGHTED was supplied during creation, true otherwise (logical).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Dist_graph_neighbors_count` and :ref:`MPI_Graph_neighbors` provide adjacency
information for a distributed graph topology.
:ref:`MPI_Dist_graph_neighbors_count` returns the number of sources and
destinations for the calling process.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Dist_graph_neighbors`
