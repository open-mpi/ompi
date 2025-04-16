.. _mpi_graph_get:

MPI_Graph_get
=============

.. include_body

:ref:`MPI_Graph_get` |mdash| Retrieves graph topology information associated with a
communicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_graph_get.rst

INPUT PARAMETERS
----------------

* ``comm`` : Communicator with graph structure (handle).
* ``maxindex`` : Length of vector index in the calling program (integer).
* ``maxedges`` : Length of vector edges in the calling program (integer).

OUTPUT PARAMETERS
-----------------

* ``index`` : Array of integers containing the graph structure (for details
   see the definition of MPI_Graph_create).
* ``edges`` : Array of integers containing the graph structure.
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Functions :ref:`MPI_Graphdims_get` and :ref:`MPI_Graph_get` retrieve the
graph-topology information that was associated with a communicator by
:ref:`MPI_Graph_create`.

The information provided by :ref:`MPI_Graphdims_get` can be used to dimension
the vectors index and edges correctly for a call to :ref:`MPI_Graph_get`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Graph_create`
