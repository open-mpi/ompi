.. _mpi_graphdims_get:

MPI_Graphdims_get
=================

.. include_body

:ref:`MPI_Graphdims_get` |mdash| Retrieves graph topology information associated with
a communicator.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GRAPHDIMS_GET(COMM, NNODES, NEDGES, IERROR)
       INTEGER COMM, NNODES, NEDGES, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Graphdims_get(comm, nnodes, nedges, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(OUT) :: nnodes, nedges
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
