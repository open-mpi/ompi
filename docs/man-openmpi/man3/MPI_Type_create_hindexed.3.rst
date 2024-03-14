.. _mpi_type_create_hindexed:

MPI_Type_create_hindexed
========================

.. include_body

:ref:`MPI_Type_create_hindexed` |mdash| Creates an indexed datatype with offsets in bytes.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_create_hindexed(int count, int *array_of_blocklengths,
        MPI_Aint *array_of_displacements, MPI_Datatype oldtype,
        MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_TYPE_CREATE_HINDEXED(COUNT, ARRAY_OF_BLOCKLENGTHS,
             ARRAY_OF_DISPLACEMENTS, OLDTYPE, NEWTYPE, IERROR)
       INTEGER COUNT, ARRAY_OF_BLOCKLENGTHS(*)
       INTEGER ARRAY_OF_DISPLACEMENTS(*), OLDTYPE, NEWTYPE
       INTEGER IERROR


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

The function is identical to :ref:`MPI_Type_indexed`, except that block
displacements in ``array_of_displacements`` are specified in bytes, rather
than in multiples of the ``oldtype`` extent.

Assume that oldtype has type map

::

       {(type(0), disp(0)), ..., (type(n-1), disp(n-1))},

with extent ex. Let B be the ``array_of_blocklength`` argument and D be the
``array_of_displacements`` argument. The newly created datatype has

::

   n x S^count-1
       (i=0)        B[i]  entries:

     {(type(0), disp(0) + D[0]),...,(type(n-1), disp(n-1) + D[0]),...,
     (type(0), disp(0) + (D[0] + B[0]-1)* ex),...,
     type(n-1), disp(n-1) + (D[0]+ B[0]-1)* ex),...,
     (type(0), disp(0) + D[count-1]),...,(type(n-1), disp(n-1) + D[count-1]),...,
     (type(0), disp(0) +  D[count-1] + (B[count-1] -1)* ex),...,
     (type(n-1), disp(n-1) + D[count-1] + (B[count-1] -1)* ex)}


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_create_struct`
   * :ref:`MPI_Type_create_hvector`
   * :ref:`MPI_Type_indexed`
