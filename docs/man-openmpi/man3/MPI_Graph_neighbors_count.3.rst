.. _mpi_graph_neighbors_count:

MPI_Graph_neighbors_count
=========================

.. include_body

:ref:`MPI_Graph_neighbors_count` |mdash| Returns the number of neighbors of a node
associated with a graph topology.

.. The following file was automatically generated
.. include:: ./bindings/mpi_graph_neighbors_count.rst

INPUT PARAMETERS
----------------

* ``comm`` : Communicator with graph topology (handle).
* ``rank`` : Rank of process in group of comm (integer).

OUTPUT PARAMETERS
-----------------

* ``nneighbors`` : Number of neighbors of specified process (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Graph_neighbors_count` and :ref:`MPI_Graph_neighbors` provide adjacency
information for a general, graph topology. :ref:`MPI_Graph_neighbors_count`
returns the number of neighbors for the process signified by rank.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Graph_neighbors`
