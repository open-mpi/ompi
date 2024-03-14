.. _mpi_type_create_indexed_block:


MPI_Type_create_indexed_block
=============================

.. include_body

:ref:`MPI_Type_create_indexed_block`, :ref:`MPI_Type_create_hindexed_block` -
Creates an indexed data type with the same block length for all blocks.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_create_indexed_block(int count, int blocklength, const int array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype)

   int MPI_Type_create_hindexed_block(int count, int blocklength, const MPI_Aint array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_CREATE_INDEXED_BLOCK(COUNT, BLOCKLENGTH,
   		ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
   	INTEGER	COUNT, BLOCKLENGTH, ARRAY_OF_DISPLACEMENTS(*),
   	        OLDTYPE, NEWTYPE, IERROR

   MPI_TYPE_CREATE_HINDEXED_BLOCK(COUNT, BLOCKLENGTH,
   		ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
   	INTEGER	COUNT, BLOCKLENGTH, OLDTYPE, NEWTYPE
   	INTEGER(KIND=MPI_ADDRESS_KIND) ARRAY_OF_DISPLACEMENTS(*)
   	INTEGER	IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_create_indexed_block(count, blocklength, array_of_displacements,
   		oldtype, newtype, ierror)
   	INTEGER, INTENT(IN) :: count, blocklength,
   	array_of_displacements(count)
   	TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Type_create_hindexed_block(count, blocklength, array_of_displacements,
   		oldtype, newtype, ierror)
   	INTEGER, INTENT(IN) :: count, blocklength
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) ::
   	array_of_displacements(count)
   	TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: Length of array of displacements (integer).
* ``blocklength``: Size of block (integer).
* ``array_of_displacements``: Array of displacements (array of integers). In units of the extent of *oldtype* for MPI_Type_create_indexed_block and bytes for MPI_Type_create_hindexed_block.
* ``oldtype``: Old data type (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New data type (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_indexed_block` and :ref:`MPI_Type_create_hindexed_block` create
an indexed data type with the same block length for all blocks. The only
difference between the two functions is :ref:`MPI_Type_create_indexed_block`
takes an array of displacements in units of the extent of *oldtype*
while :ref:`MPI_Type_create_hindexed_block` takes displacements in bytes.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_indexed`
