.. _mpi_type_create_struct:


MPI_Type_create_struct
======================

.. include_body

:ref:`MPI_Type_create_struct` |mdash| Creates a structured data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_create_struct(int count, int array_of_blocklengths[],
   	const MPI_Aint array_of_displacements[], const MPI_Datatype array_of_types[],
   	MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_CREATE_STRUCT(COUNT, ARRAY_OF_BLOCKLENGTHS,
   		ARRAY_OF_DISPLACEMENTS, ARRAY_OF_TYPES, NEWTYPE, IERROR)
   	INTEGER	COUNT, ARRAY_OF_BLOCKLENGTHS(*), ARRAY_OF_TYPES(*),
   	INTEGER NEWTYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ARRAY_OF_DISPLACEMENTS(*)


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_create_struct(count, array_of_blocklengths,
   		array_of_displacements, array_of_types, newtype, ierror)
   	INTEGER, INTENT(IN) :: count, array_of_blocklengths(count)
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) ::
   	array_of_displacements(count)
   	TYPE(MPI_Datatype), INTENT(IN) :: array_of_types(count)
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: Number of blocks (integer) |mdash| also number of entries in arrays *array_of_types*, *array_of_displacements*, and *array_of_blocklengths*.
* ``array_of_blocklengths``: Number of elements in each block (array of integers).
* ``array_of_displacements``: Byte displacement of each block (array of integers).
* ``array_of_types``: Type of elements in each block (array of handles to data-type objects).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New data type (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_struct` creates a structured data type. This routine
replaces :ref:`MPI_Type_struct`, which is now deprecated.


:ref:`MPI_Type_create_struct` is the most general type constructor. It further
generalizes :ref:`MPI_Type_create_hindexed` in that it allows each block to consist of
replications of different datatypes.

**Example 1:**
Let ``type1`` have type map

::


       {(double, 0), (char, 8)}

with extent 16. Let ``B = (2, 1, 3)``, ``D = (0, 16, 26)``, and ``T = (MPI_FLOAT, type1, MPI_CHAR)``.
Then a call to ``MPI_Type_create_struct(3, B, D, T, newtype)``
returns a datatype with type map

::


       {
        (float, 0), (float,4),             // 2 float
        (double, 16), (char, 24),          // 1 type1
        (char, 26), (char, 27), (char, 28) // 3 char
       }

That is, two copies of ``MPI_FLOAT`` starting at 0, followed by one copy of
``type1`` starting at 16, followed by three copies of ``MPI_CHAR``, starting at
26.


**Example 2:**

An example of a struct with only some components part of the type

.. code-block:: c

   struct MyStruct {
       double x[2], y;
       char a;
       int n;
   };

   // create a new type where we only send x, y and n
   int B[] = {
       2, // 2 double's
       1, // 1 double
       1, // 1 int
       1  // alignment padding
   };
   MPI_Aint D[] = {
       offsetof(struct MyStruct, x),
       offsetof(struct MyStruct, y),
       offsetof(struct MyStruct, n),
       sizeof(struct MyStruct)
   };
   MPI_Datatype T[] = {
       MPI_DOUBLE,
       MPI_DOUBLE,
       MPI_INT,
       MPI_UB
   };

   MPI_Datatype mpi_dt_mystruct;
   MPI_Type_create_struct(4, B, D, T, &mpi_dt_mystruct);
   MPI_Type_commit(&mpi_dt_mystruct);

   // We can now send a struct (omitting a)

   struct MyStruct values[3];

   if ( rank == 0 ) {
       // ... initialize values
       MPI_Send(values, 3, mpi_dt_mystruct, 1, 0, MPI_COMM_WORLD);
   } else if ( rank == 1 ) {
       MPI_Recv(values, 3, mpi_dt_mystruct, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
   }


For more information, see section 5.1.2 of the MPI-4.0 Standard.


NOTES
-----

If an upper bound is set explicitly by using the MPI datatype ``MPI_UB``, the
corresponding index must be positive.

The MPI-1 Standard originally made vague statements about padding and
alignment; this was intended to allow the simple definition of
structures that could be sent with a count greater than one. For
example,

.. code-block:: c

       struct {int a; char b;} foo;

may have

.. code-block:: c

       sizeof(foo) = sizeof(int) + sizeof(char);

defining the extent of a datatype as including an epsilon, which would
have allowed an implementation to make the extent an MPI datatype for
this structure equal to ``2*sizeof(int)``. However, since different systems
might define different paddings, a clarification to the standard made
epsilon zero. Thus, if you define a structure datatype and wish to send
or receive multiple items, you should explicitly include an ``MPI_UB`` entry
as the last member of the structure. See the above example.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_struct`
   * :ref:`MPI_Type_create_hindexed`
