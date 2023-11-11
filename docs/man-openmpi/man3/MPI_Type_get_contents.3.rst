.. _mpi_type_get_contents:


MPI_Type_get_contents
=====================

.. include_body

:ref:`MPI_Type_get_contents` |mdash| Returns information about arguments used in
creation of a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_get_contents(MPI_Datatype datatype, int max_integers,
   	int max_addresses, int max_datatypes, int array_of_integers[], MPI_Aint array_of_addresses[], MPI_Datatype array_of_datatypes[])


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_GET_CONTENTS(DATATYPE, MAX_INTEGERS, MAX_ADDRESSES,
   		MAX_DATATYPES, ARRAY_OF_INTEGERS, ARRAY_OF_ADDRESSES,
   		ARRAY_OF_DATATYPES, IERROR)
   	INTEGER	DATATYPE, MAX_INTEGERS, MAX_ADDRESSES, MAX_DATATYPES
   	INTEGER	ARRAY_OF_INTEGERS(*), ARRAY_OF_DATATYPES(*), IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) ARRAY_OF_ADDRESSES(*)


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_get_contents(datatype, max_integers, max_addresses, max_datatypes,
   	array_of_integers, array_of_addresses, array_of_datatypes,
   		ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, INTENT(IN) :: max_integers, max_addresses, max_datatypes
   	INTEGER, INTENT(OUT) :: array_of_integers(max_integers)
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) ::
   	array_of_addresses(max_addresses)
   	TYPE(MPI_Datatype), INTENT(OUT) :: array_of_datatypes(max_datatypes)
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``datatype``: Data type to access (handle).
* ``max_integers``: Number of elements in *array_of_integers (nonnegative integer).*
* ``max_addresses``: Number of elements in *array_of_addresses (nonnegative integer).*
* ``max_datatypes``: Number of elements in *array_of_datatypes (nonnegative integer).*

OUTPUT PARAMETERS
-----------------
* ``array_of_integers``: Contains integer arguments used in constructing *datatype (array of integers).*
* ``array_of_addresses``: Contains address arguments used in constructing *datatype (array of integers).*
* ``array_of_datatypes``: Contains data-type arguments used in constructing *datatype (array of integers).*
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

For the given data type, :ref:`MPI_Type_get_envelope` returns information on
the number and type of input arguments used in the call that created the
data type. The number-of-arguments values returned can be used to
provide sufficiently large arrays in the decoding routine
:ref:`MPI_Type_get_contents`. This call and the meaning of the returned values
is described below. The combiner reflects the MPI data type constructor
call that was used in creating *datatype.*

The parameter *datatype must be a predefined unnamed or a derived data
type. The call is erroneous if datatype is a predefined named data
type.*

The values given for *max_integers, max_addresses, and max_datatypes
must be at least as large as the value returned in num_integers,
num_addresses, and num_datatypes, respectively, in the call
:ref:`MPI_Type_get_envelope` for the same datatype argument.*

The data types returned in *array_of_datatypes are handles to data-type
objects that are equivalent to the data types used in the original
construction call. If these were derived data types, then the returned
data types are new data-type objects, and the user is responsible for
freeing these datatypes with :ref:`MPI_Type_free`. If these were predefined
data types, then the returned data type is equal to that (constant)
predefined data type and cannot be freed.*

The committed state of returned derived data types is undefined, that
is, the data types may or may not be committed. Furthermore, the content
of attributes of returned data types is undefined.

Note that :ref:`MPI_Type_get_contents` can be invoked with a data-type argument
that was constructed using :ref:`MPI_Type_create_f90_real`,
:ref:`MPI_Type_create_f90_integer`, or :ref:`MPI_Type_create_f90_complex` (an unnamed
predefined data type). In such a case, an empty *array_of_datatypes* is
returned.

In the legacy MPI-1 datatype constructor calls, the address arguments in
Fortran are of type ``INTEGER``. In subsequent versions of the `MPI
Standard <https://www.mpi-forum.org/docs/>`_, the address
arguments are of type ``INTEGER(KIND=MPI_ADDRESS_KIND)``. The call
:ref:`MPI_Type_get_contents` returns all addresses in an argument of type
``INTEGER(KIND=MPI_ADDRESS_KIND)``. This is true even if the old MPI-1 calls
were used. Thus, the location of values returned can be thought of as
being returned by the C bindings. It can also be determined by examining
the current MPI calls for datatype constructors for the deprecated MPI-1
calls that involve addresses.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_get_envelope`
