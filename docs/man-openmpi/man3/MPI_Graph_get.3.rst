.. _mpi_graph_get:

MPI_Graph_get
=============

.. include_body

:ref:`MPI_Graph_get` - Retrieves graph topology information associated with a
communicator.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Graph_get(MPI_Comm comm, int maxindex, int maxedges,
       int index[], int edges[])

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GRAPH_GET(COMM, MAXINDEX, MAXEDGES, INDEX, EDGES, IERROR)
       INTEGER COMM, MAXINDEX, MAXEDGES, INDEX(*)
       INTEGER EDGES(*), IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Graph_get(comm, maxindex, maxedges, index, edges, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: maxindex, maxedges
       INTEGER, INTENT(OUT) :: index(maxindex), edges(maxedges)
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  comm : Communicator with graph structure (handle).
-  maxindex : Length of vector index in the calling program (integer).
-  maxedges : Length of vector edges in the calling program (integer).

Output Parameters
-----------------

-  index : Array of integers containing the graph structure (for details
   see the definition of MPI_Graph_create).
-  edges : Array of integers containing the graph structure.
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

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso:: :ref:`MPI_Graph_create`
