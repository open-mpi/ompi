.. _mpi_type_create_resized:


MPI_Type_create_resized
=======================

.. include_body

:ref:`MPI_Type_create_resized` |mdash| Returns a new data type with new extent
and upper and lower bounds.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb,
   	MPI_Aint extent, MPI_Datatype *newtype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_CREATE_RESIZED(OLDTYPE, LB, EXTENT, NEWTYPE, IERROR)
   	INTEGER	OLDTYPE, NEWTYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND)	LB, EXTENT


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_create_resized(oldtype, lb, extent, newtype, ierror)
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: lb, extent
   	TYPE(MPI_Datatype), INTENT(IN) :: oldtype
   	TYPE(MPI_Datatype), INTENT(OUT) :: newtype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``oldtype``: Input data type (handle).
* ``lb``: New lower bound of data type (integer).
* ``extent``: New extent of data type (integer).

OUTPUT PARAMETERS
-----------------
* ``newtype``: Output data type (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_resized` returns in *newtype* a handle to a new data type
that is identical to *oldtype*, except that the lower bound of this new
data type is set to be *lb*, and its upper bound is set to be *lb* +
*extent*. Any previous *lb* and *ub* markers are erased, and a new pair
of lower bound and upper bound markers are put in the positions
indicated by the *lb* and *extent* arguments. This affects the behavior
of the data type when used in communication operations, with *count* >
1, and when used in the construction of new derived data types.


NOTE
----

Use of :ref:`MPI_Type_create_resized` is strongly recommended over the legacy
MPI-1 functions :ref:`MPI_Type_extent` and :ref:`MPI_Type_lb`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_get_extent`
