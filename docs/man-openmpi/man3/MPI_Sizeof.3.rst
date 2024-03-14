.. _mpi_sizeof:


MPI_Sizeof
==========

.. include_body

:ref:`MPI_Sizeof` |mdash| Returns the size, in bytes, of the given type


SYNTAX
------


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SIZEOF(X, SIZE, IERROR)
   <type>	X
   INTEGER	SIZE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Sizeof(x, size, ierror)
   	TYPE(*), DIMENSION(..) :: x
   	INTEGER, INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``X``: A Fortran variable of numeric intrinsic type (choice).

OUTPUT PARAMETERS
-----------------
* ``SIZE``: Size of machine representation of that type (integer).
* ``ierror``: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_SIZEOF` returns the size (in bytes) of the machine representation of
the given variable. It is a generic Fortran type and has a Fortran
binding only. This routine is similar to the sizeof builtin in C.
However, if given an array argument, it returns the size of the base
element, not the size of the whole array.


NOTES
-----

This function is not available in C because it is not necessary.


ERRORS
------

.. include:: ./ERRORS.rst
