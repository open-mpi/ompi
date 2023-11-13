.. _mpi_type_hindexed:


MPI_Type_hindexed
=================

.. include_body

:ref:`MPI_Type_hindexed` |mdash| Creates an indexed datatype with offsets in bytes |mdash| |deprecated_favor| :ref:`MPI_Type_create_hindexed`.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_hindexed(int count, int *array_of_blocklengths,
   	MPI_Aint *array_of_displacements, MPI_Datatype oldtype,
   	MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_TYPE_HINDEXED(COUNT, ARRAY_OF_BLOCKLENGTHS,
   		ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
   	INTEGER	COUNT, ARRAY_OF_BLOCKLENGTHS(*)
   	INTEGER	ARRAY_OF_DISPLACEMENTS(*), OLDTYPE, NEWTYPE
   	INTEGER	IERROR


INPUT PARAMETERS
----------------
* ``count``: Number of blocks |mdash| also number of entries in array_of_displacements and array_of_blocklengths (integer).
* ``array_of_blocklengths``: Number of elements in each block (array of nonnegative integers).
* ``array_of_displacements``: Byte displacement of each block (C: array of *MPI_Aint*, Fortran: array of integer).
* ``oldtype``: Old datatype (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New datatype (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Use
:ref:`MPI_Type_create_hindexed` instead.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_create_hindexed`
   * :ref:`MPI_Type_create_struct`
   * :ref:`MPI_Type_indexed`
