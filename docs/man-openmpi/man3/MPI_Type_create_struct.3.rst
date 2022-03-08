.. _mpi_type_create_struct:


MPI_Type_create_struct
======================

.. include_body

:ref:`MPI_Type_create_struct` - Creates a structured data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_create_struct(int count, int array_of_blocklengths[],
   	const MPI_Aint array_of_displacements[], const MPI_Datatype array_of_types[],
   	MPI_Datatype *newtype)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
* ``count``: Number of blocks (integer) -- also number of entries in arrays *array_of_types*, *array_of_displacements*, and *array_of_blocklengths*.
* ``array_of_blocklengths``: Number of elements in each block (array of integers).
* ``array_of_displacements``: Byte displacement of each block (array of integers).
* ``array_of_types``: Type of elements in each block (array of handles to data-type objects).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New data type (handle).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_struct` creates a structured data type. This routine
replaces :ref:`MPI_Type_struct`, which is now deprecated.

NOTE - This routine replaces :ref:`MPI_Type_struct`, which is deprecated. See
the man page :ref:`MPI_Type_struct` for information about that routine.


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the
*ARRAY_OF_DISPLACEMENTS*\ (*) argument only for Fortran 90. FORTRAN 77
users may use the non-portable syntax

::

        INTEGER*MPI_ADDRESS_KIND ARRAY_OF_DISPLACEMENTS(*)

where MPI_ADDRESS_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.


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
   :ref:`MPI_Type_struct` :ref:`MPI_Type_create_hindexed`
