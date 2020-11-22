.. _mpi_type_struct:


MPI_Type_struct
===============

.. include_body

:ref:`MPI_Type_struct` - Creates a *struct* data type -- use of this
routine is deprecated.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_struct(int count, int *array_of_blocklengths,
   	MPI_Aint *array_of_displacements, MPI_Datatype *array_of_types,
   	MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   INCLUDE 'mpif.h'
   MPI_TYPE_STRUCT(COUNT, ARRAY_OF_BLOCKLENGTHS,
   		ARRAY_OF_DISPLACEMENTS, ARRAY_OF_TYPES,
   		NEWTYPE, IERROR)
   	INTEGER	COUNT, ARRAY_OF_BLOCKLENGTHS(*)
   	INTEGER	ARRAY_OF_DISPLACEMENTS(*)
   	INTEGER	ARRAY_OF_TYPES(*), NEWTYPE, IERROR


INPUT PARAMETERS
----------------
* ``count``: Number of blocks (integer) also number of entries in arrays array_of_types, array_of_displacements, and array_of_blocklengths.
* ``array_of_blocklengths``: Number of elements in each block (array).
* ``array_of_displacements``: Byte displacement of each block (array).
* ``array_of_types``: Type of elements in each block (array of handles to datatype objects).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New datatype (handle).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Use
:ref:`MPI_Type_create_struct` instead.

:ref:`MPI_Type_struct` is the most general type constructor. It further
generalizes :ref:`MPI_Type_hindexed` in that it allows each block to consist of
replications of different datatypes.

**Example:** Let type1 have type map

::


       {(double, 0), (char, 8)}

with extent 16. Let B = (2, 1, 3), D = (0, 16, 26), and T = (MPI_FLOAT,
type1, MPI_CHAR). Then a call to MPI_Type_struct(3, B, D, T, newtype)
returns a datatype with type map

::


       {(float, 0), (float,4), (double, 16), (char, 24),
       (char, 26), (char, 27), (char, 28)}

That is, two copies of MPI_FLOAT starting at 0, followed by one copy of
type1 starting at 16, followed by three copies of MPI_CHAR, starting at
26. (We assume that a float occupies 4 bytes.)

For more information, see section 3.12.1 of the MPI-1.1 Standard.


NOTES
-----

If an upperbound is set explicitly by using the MPI datatype MPI_UB, the
corresponding index must be positive.

The MPI-1 Standard originally made vague statements about padding and
alignment; this was intended to allow the simple definition of
structures that could be sent with a count greater than one. For
example,

::

       struct {int a; char b;} foo;

may have

::

       sizeof(foo) = sizeof(int) + sizeof(char);

defining the extent of a datatype as including an epsilon, which would
have allowed an implementation to make the extent an MPI datatype for
this structure equal to 2*sizeof(int). However, since different systems
might define different paddings, a clarification to the standard made
epsilon zero. Thus, if you define a structure datatype and wish to send
or receive multiple items, you should explicitly include an MPI_UB entry
as the last member of the structure. For example, the following code can
be used for the structure foo:

::


       blen[0] = 1; indices[0] = 0; oldtypes[0] = MPI_INT;
       blen[1] = 1; indices[1] = &foo.b - &foo; oldtypes[1] = MPI_CHAR;
       blen[2] = 1; indices[2] = sizeof(foo); oldtypes[2] = MPI_UB;
       MPI_Type_struct( 3, blen, indices, oldtypes, &newtype );


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Type_create_struct` :ref:`MPI_Type_create_hindexed`
