.. _mpi_graph_get:

MPI_Graph_get
=============

.. include_body

:ref:`MPI_Graph_get` |mdash| Retrieves graph topology information associated with a
communicator.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Graph_get(MPI_Comm comm, int maxindex, int maxedges,
       int index[], int edges[])

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GRAPH_GET(COMM, MAXINDEX, MAXEDGES, INDEX, EDGES, IERROR)
       INTEGER COMM, MAXINDEX, MAXEDGES, INDEX(*)
       INTEGER EDGES(*), IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Graph_get(comm, maxindex, maxedges, index, edges, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: maxindex, maxedges
       INTEGER, INTENT(OUT) :: index(maxindex), edges(maxedges)
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
