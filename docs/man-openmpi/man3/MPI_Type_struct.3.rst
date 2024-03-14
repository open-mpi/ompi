.. _mpi_type_struct:


MPI_Type_struct
===============

.. include_body

:ref:`MPI_Type_struct` |mdash| Creates a *struct* data type |mdash| |deprecated_favor| :ref:`MPI_Type_create_struct`.


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

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

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
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2.
For details of use, see :ref:`MPI_Type_create_struct`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_create_struct`
   * :ref:`MPI_Type_create_hindexed`
