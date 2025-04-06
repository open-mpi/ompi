.. _mpi_graph_map:

MPI_Graph_map
=============

.. include_body

:ref:`MPI_Graph_map` |mdash| Maps process to graph topology information.

.. The following file was automatically generated
.. include:: ./bindings/mpi_graph_map.rst

INPUT PARAMETERS
----------------

* ``comm`` : Input communicator (handle).
* ``nnodes`` : Number of graph nodes (integer).
* ``index`` : Integer array specifying the graph structure, see
   :ref:`MPI_Graph_create`.
* ``edges`` : Integer array specifying the graph structure.

OUTPUT PARAMETERS
-----------------

* ``newrank`` : Reordered rank of the calling process; MPI_UNDEFINED if the
   calling process does not belong to graph (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Cart_map` and :ref:`MPI_Graph_map` can be used to implement all other
topology functions. In general they will not be called by the user
directly, unless he or she is creating additional virtual topology
capability other than that provided by MPI.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Cart_map`
