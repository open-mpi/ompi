.. _mpi_graph_map:

MPI_Graph_map
=============

.. include_body

:ref:`MPI_Graph_map` |mdash| Maps process to graph topology information.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Graph_map(MPI_Comm comm, int nnodes, const int index[],
       const int edges[], int *newrank)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GRAPH_MAP(COMM, NNODES, INDEX, EDGES, NEWRANK, IERROR)
       INTEGER COMM, NNODES, INDEX(*), EDGES(*), NEWRANK, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Graph_map(comm, nnodes, index, edges, newrank, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
       INTEGER, INTENT(OUT) :: newrank
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
