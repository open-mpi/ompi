.. _mpi_graph_map:

MPI_Graph_map
=============

.. include_body

:ref:`MPI_Graph_map` - Maps process to graph topology information.

Syntax
------

C Syntax
^^^^^^^^

.. code:: c

   #include <mpi.h>

   int MPI_Graph_map(MPI_Comm comm, int nnodes, const int index[],
       const int edges[], int *newrank)

Fortran Syntax
^^^^^^^^^^^^^^

.. code:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GRAPH_MAP(COMM, NNODES, INDEX, EDGES, NEWRANK, IERROR)
       INTEGER COMM, NNODES, INDEX(*), EDGES(*), NEWRANK, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code:: fortran

   USE mpi_f08

   MPI_Graph_map(comm, nnodes, index, edges, newrank, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
       INTEGER, INTENT(OUT) :: newrank
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

Input Parameters
----------------

-  comm : Input communicator (handle).
-  nnodes : Number of graph nodes (integer).
-  index : Integer array specifying the graph structure, see
   :ref:`MPI_Graph_create`.
-  edges : Integer array specifying the graph structure.

Output Parameters
-----------------

-  newrank : Reordered rank of the calling process; MPI_UNDEFINED if the
   calling process does not belong to graph (integer).
-  IERROR : Fortran only: Error status (integer).

Description
-----------

:ref:`MPI_Cart_map` and :ref:`MPI_Graph_map` can be used to implement all other
topology functions. In general they will not be called by the user
directly, unless he or she is creating additional virtual topology
capability other than that provided by MPI.

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


.. seealso:: :ref:`MPI_Cart_map`
