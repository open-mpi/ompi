.. _mpi_type_contiguous:


MPI_Type_contiguous
===================

.. include_body

:ref:`MPI_Type_contiguous` |mdash| Creates a contiguous datatype.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_contiguous(int count, MPI_Datatype oldtype,
   	MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_CONTIGUOUS(COUNT, OLDTYPE, NEWTYPE, IERROR)
   	INTEGER	COUNT, OLDTYPE, NEWTYPE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_contiguous(count, oldtype, newtype, ierror)
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``count``: Replication count (nonnegative integer).
* ``oldtype``: Old datatype (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New datatype (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The simplest datatype constructor is :ref:`MPI_Type_contiguous`, which allows
replication of a datatype into contiguous locations.

*newtype* is the datatype obtained by concatenating *count* copies of
*oldtype*. Concatenation is defined using the extent of *oldtype* as the
size of the concatenated copies.

**Example:** Let oldtype have type map {(double, 0), (char, 8)}, with
extent 16, and let count = 3. The type map of the datatype returned by
newtype is

.. code-block:: c

       {(double, 0), (char, 8), (double, 16), (char, 24),
       (double, 32), (char, 40)];

i.e., alternating double and char elements, with displacements 0, 8, 16,
24, 32, 40.

In general, assume that the type map of oldtype is

.. code-block:: c

       {(type(0), disp(0)),...,(type(n-1), disp(n-1))},

with extent ex. Then newtype has a type map with count times n entries
defined by:

.. code-block:: c

       {(type(0), disp(0)), ...,(type(n-1), disp(n-1)),
       (type(0), disp(0) + ex), ...,(type(n-1),
       disp(n-1) + ex), ...,(type(0), disp(0) + ex * (count - 1)),
       ...,(type(n-1), disp(n-1) + ex * (count - 1))}.

For more information about derived datatypes, see Section 3.12 of the
MPI-1 Standard.


ERRORS
------

.. include:: ./ERRORS.rst
