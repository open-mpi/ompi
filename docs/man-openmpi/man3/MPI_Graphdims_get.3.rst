.. _mpi_graphdims_get:

MPI_Graphdims_get
=================

.. include_body

:ref:`MPI_Graphdims_get` - Retrieves graph topology information associated with
a communicator.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GRAPHDIMS_GET(COMM, NNODES, NEDGES, IERROR)
       INTEGER COMM, NNODES, NEDGES, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Graphdims_get(comm, nnodes, nedges, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(OUT) :: nnodes, nedges
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameter
---------------

-  comm : Communicator for group with graph structure (handle).

Output Parameters
-----------------

-  nnodes : Number of nodes in graph (integer).
-  nedges : Number of edges in graph (integer).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

Functions :ref:`MPI_Graphdims_get` and :ref:`MPI_Graph_get` retrieve the
graph-topology information that was associated with a communicator by
:ref:`MPI_Graph_create`.

The information provided by :ref:`MPI_Graphdims_get` can be used to dimension
the vectors index and edges correctly for a call to :ref:`MPI_Graph_get`.

Errors
------

.. include:: ./ERRORS.rst

.. seealso:: :ref:`MPI_Graph_create`
