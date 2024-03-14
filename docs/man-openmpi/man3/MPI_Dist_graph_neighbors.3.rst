.. _mpi_dist_graph_neighbors:


MPI_Dist_graph_neighbors
========================

.. include_body

:ref:`MPI_Dist_graph_neighbors` |mdash| Returns the neighbors of the calling
process in a distributed graph topology.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Dist_graph_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[],
                                int maxoutdegree, int destinations[], int destweights[])


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_DIST_GRAPH_NEIGHBORS(COMM, MAXINDEGREE, SOURCES, SOURCEWEIGHTS,
           MAXOUTDEGREE, DESTINATIONS, DESTWEIGHTS, IERROR)
           INTEGER COMM, MAXINDEGREE, SOURCES(*), SOURCEWEIGHTS(*), MAXOUTDEGREE,
                   DESTINATIONS(*), DESTWEIGHTS(*), IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Dist_Graph_neighbors(comm, maxindegree, sources, sourceweights,
   		maxoutdegree, destinations, destweights, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
   	INTEGER, INTENT(OUT) :: sources(maxindegree), destinations(maxoutdegree)
   	INTEGER :: sourceweights(*), destweights(*)
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``comm``: Communicator with distributed graph topology (handle).
* ``maxindegree``: Size of *sources* and *sourceweights* arrays (non-negative integer).
* ``maxoutdegree``: Size of *destinations* and *destweights* arrays (non-negative integer).

OUTPUT PARAMETERS
-----------------
* ``sources``: Processes for which the calling process is a destination (array of non-negative integers).
* ``sourceweights``: Weights of the edges into the calling process (array of non-negative integers).
* ``destinations``: Processes for which the calling process is a source (array of non-negative integers).
* ``destweights``: Weights of the edges out of the calling process (array of non-negative integers).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Dist_graph_neighbors` returns the source and destination ranks in a
distributed graph topology for the calling process. This call will
return up to *maxindegree* source ranks in the *sources* array and up to
*maxoutdegree* destination ranks in the *destinations* array. If weights
were specified at the time of the communicator's creation then the
associated weights are returned in the *sourceweights* and *destweights*
arrays. If the communicator was created with
:ref:`MPI_Dist_graph_create_adjacent` then the order of the values in *sources*
and *destinations* is identical to the input that was used by the
process with the same rank in comm_old in the creation call.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Dist_graph_neighbors_count`
