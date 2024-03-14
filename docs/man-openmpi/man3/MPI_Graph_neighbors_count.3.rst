.. _mpi_graph_neighbors_count:

MPI_Graph_neighbors_count
=========================

.. include_body

:ref:`MPI_Graph_neighbors_count` |mdash| Returns the number of neighbors of a node
associated with a graph topology.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Graph_neighbors_count(MPI_Comm comm, int rank,
       int *nneighbors)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GRAPH_NEIGHBORS_COUNT(COMM, RANK, NNEIGHBORS, IERROR)
       INTEGER COMM, RANK, NNEIGHBORS, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Graph_neighbors_count(comm, rank, nneighbors, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: rank
       INTEGER, INTENT(OUT) :: nneighbors
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
