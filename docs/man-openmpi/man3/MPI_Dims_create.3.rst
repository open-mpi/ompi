.. _mpi_dims_create:


MPI_Dims_create
===============

.. include_body

:ref:`MPI_Dims_create` |mdash| Creates a division of processors in a Cartesian
grid.

.. The following file was automatically generated
.. include:: ./bindings/mpi_dims_create.rst

INPUT PARAMETERS
----------------
* ``nnodes``: Number of nodes in a grid (integer).
* ``ndims``: Number of Cartesian dimensions (integer).

IN/OUT PARAMETER
----------------
* ``dims``: Integer array of size ndims specifying the number of nodes in each dimension.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

For Cartesian topologies, the function :ref:`MPI_Dims_create` helps the user
select a balanced distribution of processes per coordinate direction,
depending on the number of processes in the group to be balanced and
optional constraints that can be specified by the user. One use is to
partition all the processes (the size of MPI_COMM_WORLD's group) into an
n-dimensional topology.

The entries in the array *dims* are set to describe a Cartesian grid
with *ndims* dimensions and a total of *nnodes* nodes. The dimensions
are set to be as close to each other as possible, using an appropriate
divisibility algorithm. The caller may further constrain the operation
of this routine by specifying elements of array dims. If dims[i] is set
to a positive number, the routine will not modify the number of nodes in
dimension i; only those entries where dims[i] = 0 are modified by the
call.

Negative input values of dims[i] are erroneous. An error will occur if
nnodes is not a multiple of ((pi) over (i, dims[i] != 0)) dims[i].

For dims[i] set by the call, dims[i] will be ordered in nonincreasing
order. Array dims is suitable for use as input to routine
:ref:`MPI_Cart_create`. :ref:`MPI_Dims_create` is local.

**Example:**

::


   dims
   before					dims
   call		function call		on return
   -----------------------------------------------------
   (0,0)	MPI_Dims_create(6, 2, dims)	(3,2)
   (0,0)	MPI_Dims_create(7, 2, dims) 	(7,1)
   (0,3,0)	MPI_Dims_create(6, 3, dims)	(2,3,1)
   (0,3,0)	MPI_Dims_create(7, 3, dims)	erroneous call
   ------------------------------------------------------


ERRORS
------

.. include:: ./ERRORS.rst
