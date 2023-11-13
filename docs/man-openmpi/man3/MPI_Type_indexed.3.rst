.. _mpi_type_indexed:


MPI_Type_indexed
================

.. include_body

:ref:`MPI_Type_indexed`, :ref:`MPI_Type_create_hindexed` - Creates an indexed
datatype.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_indexed(int count, const int array_of_blocklengths[],
   	const int array_of_displacements[], MPI_Datatype oldtype,
   	MPI_Datatype *newtype)

   int MPI_Type_create_hindexed(int count,
   	const int array_of_blocklengths[],
   	const MPI_Aint array_of_displacements[], MPI_Datatype oldtype,
   	MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_INDEXED(COUNT, ARRAY_OF_BLOCKLENGTHS,
   		ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
   	INTEGER	COUNT, ARRAY_OF_BLOCKLENGTHS(*)
   	INTEGER	ARRAY_OF_DISPLACEMENTS(*), OLDTYPE, NEWTYPE
   	INTEGER	IERROR

   MPI_TYPE_CREATE_HINDEXED(COUNT, ARRAY_OF_BLOCKLENGTHS,
   		ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
   	INTEGER	COUNT, ARRAY_OF_BLOCKLENGTHS(*)
   	INTEGER	OLDTYPE, NEWTYPE
   	INTEGER(KIND=MPI_ADDRESS_KIND) ARRAY_OF_DISPLACEMENTS(*)
   	INTEGER	IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_indexed(count, array_of_blocklengths, array_of_displacements,
   		oldtype, newtype, ierror)
   	INTEGER, INTENT(IN) :: count, array_of_blocklengths(count),
   	array_of_displacements(count)
   	TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Type_create_hindexed(count, array_of_blocklengths,
   		array_of_displacements, oldtype, newtype, ierror)
   	INTEGER, INTENT(IN) :: count, array_of_blocklengths(count)
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) ::
   	array_of_displacements(count)
   	TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: Number of blocks |mdash| also number of entries in array_of_displacements and array_of_blocklengths (nonnegative integer).
* ``array_of_blocklengths``: Number of elements per block (array of nonnegative integers).
* ``array_of_displacements``: Displacement for each block, in multiples of oldtype extent for MPI_Type_indexed and bytes for MPI_Type_create_hindexed (array of integer for **MPI_TYPE_INDEXED**, array of *MPI_Aint* for **MPI_TYPE_CREATE_HINDEXED**).
* ``oldtype``: Old datatype (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New datatype (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Type_indexed` allows replication of an old datatype into
a sequence of blocks (each block is a concatenation of the old
datatype), where each block can contain a different number of copies and
have a different displacement. All block displacements are multiples of
the old data type's extent.

**Example:** Let oldtype have type map {(double, 0), (char, 8)}, with
extent 16. Let B = (3, 1) and let D = (4, 0). A call to
MPI_Type_indexed(2, B, D, oldtype, newtype) returns a datatype with type
map

::

       {(double, 64), (char, 72), (double, 80), (char, 88),
       (double, 96), (char, 104),
       (double, 0), (char, 8)}

That is, three copies of the old type starting at displacement 4 x 16 =
64, and one copy starting at displacement 0.

In general, assume that oldtype has type map

::

       {(type(0), disp(0)), ..., (type(n-1), disp(n-1))},

| with extent ex. Let B be the array_of_blocklength argument and D be
  the array_of_displacements argument. The newly created datatype has

::

   n x S ^count-1
       i = 0           B[i]  entries:

       {(type(0), disp(0) + D[0]* ex), ...,
       (type(n-1), disp(n-1) + D[0]* ex), ...,
       (type(0), disp(0) + (D[0] + B[0]-1)* ex), ...,
       (type(n-1), disp(n-1) + (D[0]+ B[0]-1)* ex), ...,
       (type(0), disp(0) + D[count-1]* ex), ...,
       (type(n-1), disp(n-1) + D[count-1]* ex), ...,
       (type(0), disp(0) +  (D[count-1] + B[count-1] -1)* ex), ...,
       (type(n-1), disp(n-1) + (D[count-1] + B[count-1] -1)* ex)}

A call to MPI_Type_vector(count, blocklength, stride, oldtype, newtype)
is equivalent to a call to MPI_Type_indexed(count, B, D, oldtype,
newtype) where

::

       D[j] = j * stride, j = 0,..., count-1

   and

       B[j] = blocklength, j = 0, .., count-1

The function :ref:`MPI_Type_create_hindexed` is identical to :ref:`MPI_Type_indexed`,
except that block displacements in *array_of_displacements* are
specified in bytes, rather than in multiples of the *oldtype* extent.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_hindexed`
