.. _mpi_type_create_darray:


MPI_Type_create_darray
======================

.. include_body

:ref:`MPI_Type_create_darray` |mdash| Creates a distributed array datatype;


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_create_darray(int size, int rank, int ndims,
   	const int array_of_gsizes[], const int array_of_distribs[],
   	const int array_of_dargs[], const int array_of_psizes[],
   	int order, MPI_Datatype oldtype, MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_CREATE_DARRAY(SIZE, RANK, NDIMS, ARRAY_OF_GSIZES,
   	ARRAY_OF_DISTRIBS, ARRAY_OF_DARGS, ARRAY_OF_PSIZES, ORDER,
   	OLDTYPE, NEWTYPE, IERROR)

   	INTEGER	SIZE, RANK, NDIMS, ARRAY_OF_GSIZES(*), ARRAY_OF_DISTRIBS(*),
   		ARRAY_OF_DARGS(*), ARRAY_OF_PSIZES(*), ORDER, OLDTYPE,
   		NEWTYPE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_create_darray(size, rank, ndims, array_of_gsizes,
   	array_of_distribs, array_of_dargs, array_of_psizes, order,
   		oldtype, newtype, ierror)
   	INTEGER, INTENT(IN) :: size, rank, ndims, array_of_gsizes(ndims),
   	array_of_distribs(ndims), array_of_dargs(ndims),
   	array_of_psizes(ndims), order
   	TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``size``: Size of process group (positive integer).
* ``rank``: Rank in process group (nonnegative integer).
* ``ndims``: Number of array dimensions as well as process grid dimensions (positive integer).
* ``array_of_gsizes``: Number of elements of type *oldtype* in each dimension of global array (array of positive integers).
* ``array_of_distribs``: Distribution of array in each dimension (array of state).
* ``array_of_dargs``: Distribution argument in each dimension (array of positive integers).
* ``array_of_psizes``: Size of process grid in each dimension (array of positive integers).
* ``order``: Array storage order flag (state).
* ``oldtype``: Old data type (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New data type (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_darray` can be used to generate the data types
corresponding to the distribution of an ndims-dimensional array of
*oldtype* elements onto an *ndims*-dimensional grid of logical
processes. Unused dimensions of *array_of_psizes* should be set to 1.
For a call to :ref:`MPI_Type_create_darray` to be correct, the equation

::

       ndims-1
     pi              array_of_psizes[i] = size
       i=0

must be satisfied. The ordering of processes in the process grid is
assumed to be row-major, as in the case of virtual Cartesian process
topologies in MPI-1.

Each dimension of the array can be distributed in one of three ways:

* ``MPI_DISTRIBUTE_BLOCK`` - Block distribution
* ``MPI_DISTRIBUTE_CYCLIC`` - Cyclic distribution
* ``MPI_DISTRIBUTE_NONE`` - Dimension not distributed.

The constant MPI_DISTRIBUTE_DFLT_DARG specifies a default distribution
argument. The distribution argument for a dimension that is not
distributed is ignored. For any dimension *i* in which the distribution
is MPI_DISTRIBUTE_BLOCK, it erroneous to specify *array_of_dargs[i]*
*\** *array_of_psizes[i]* < *array_of_gsizes[i]*.

For example, the HPF layout ARRAY(CYCLIC(15)) corresponds to
MPI_DISTRIBUTE_CYCLIC with a distribution argument of 15, and the HPF
layout ARRAY(BLOCK) corresponds to MPI_DISTRIBUTE_BLOCK with a
distribution argument of MPI_DISTRIBUTE_DFLT_DARG.

The *order* argument is used as in :ref:`MPI_TYPE_CREATE_SUBARRAY` to specify
the storage order. Therefore, arrays described by this type constructor
may be stored in Fortran (column-major) or C (row-major) order. Valid
values for order are MPI_ORDER_FORTRAN and MPI_ORDER_C.

This routine creates a new MPI data type with a typemap defined in terms
of a function called "cyclic()" (see below).

Without loss of generality, it suffices to define the typemap for the
MPI_DISTRIBUTE_CYCLIC case where ``MPI_DISTRIBUTE_DFLT_DARG`` is not used.

MPI_DISTRIBUTE_BLOCK and MPI_DISTRIBUTE_NONE can be reduced to the
MPI_DISTRIBUTE_CYCLIC case for dimension *i* as follows.

MPI_DISTRIBUTE_BLOCK with *array_of_dargs[i]* equal to
MPI_DISTRIBUTE_DFLT_DARG is equivalent to MPI_DISTRIBUTE_CYCLIC with
*array_of_dargs[i]* set to

.. code-block:: c

      (array_of_gsizes[i] + array_of_psizes[i] - 1)/array_of_psizes[i]

If *array_of_dargs[i]* is not MPI_DISTRIBUTE_DFLT_DARG, then
MPI_DISTRIBUTE_BLOCK and DISTRIBUTE_CYCLIC are equivalent.

MPI_DISTRIBUTE_NONE is equivalent to MPI_DISTRIBUTE_CYCLIC with
*array_of_dargs[i]* set to *array_of_gsizes[i]*.

Finally, MPI_DISTRIBUTE_CYCLIC with *array_of_dargs[i]* equal to
MPI_DISTRIBUTE_DFLT_DARG is equivalent to MPI_DISTRIBUTE_CYCLIC with
*array_of_dargs[i]* set to 1.


NOTES
-----

For both Fortran and C arrays, the ordering of processes in the process
grid is assumed to be row-major. This is consistent with the ordering
used in virtual Cartesian process topologies in MPI-1. To create such
virtual process topologies, or to find the coordinates of a process in
the process grid, etc., users may use the corresponding functions
provided in MPI-1.


ERRORS
------

.. include:: ./ERRORS.rst
