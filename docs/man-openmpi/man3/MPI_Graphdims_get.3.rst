.. _mpi_graphdims_get:

MPI_Graphdims_get
=================

.. include_body

:ref:`MPI_Graphdims_get` |mdash| Retrieves graph topology information associated with
a communicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_graphdims_get.rst

INPUT PARAMETER
---------------

* ``comm`` : Communicator for group with graph structure (handle).

OUTPUT PARAMETERS
-----------------

* ``nnodes`` : Number of nodes in graph (integer).
* ``nedges`` : Number of edges in graph (integer).
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
