.. _mpi_type_get_true_extent:


MPI_Type_get_true_extent
========================

.. include_body

:ref:`MPI_Type_get_true_extent`, :ref:`MPI_Type_get_true_extent_x` - Returns
the true lower bound and extent of a data type's corresponding typemap,
ignoring MPI_UB and MPI_LB markers.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_get_true_extent(MPI_Datatype datatype,
   	MPI_Aint *true_lb, MPI_Aint *true_extent)
   int MPI_Type_get_true_extent_x(MPI_Datatype datatype,
   	MPI_Count *true_lb, MPI_Count *true_extent)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_GET_TRUE_EXTENT(DATATYPE, TRUE_LB, TRUE_EXTENT, IERROR)
   	INTEGER	DATATYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) TRUE_LB, TRUE_EXTENT
   MPI_TYPE_GET_TRUE_EXTENT_X(DATATYPE, TRUE_LB, TRUE_EXTENT, IERROR)
   	INTEGER	DATATYPE, IERROR
   	INTEGER(KIND=MPI_COUNT_KIND) TRUE_LB, TRUE_EXTENT


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_get_true_extent(datatype, true_lb, true_extent, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: true_lb, true_extent
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   MPI_Type_get_true_extent_x(datatype, true_lb, true_extent, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER(KIND = MPI_COUNT_KIND), INTENT(OUT) :: true_lb, true_extent
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``datatype``: Data type for which information is wanted (handle).

OUTPUT PARAMETERS
-----------------
* ``true_lb``: True lower bound of data type (integer).
* ``true_extent``: True size of data type (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The *true_lb* parameter returns the offset of the lowest unit of store
that is addressed by the data type, that is, the lower bound of the
corresponding typemap, ignoring MPI_LB markers. The *true_extent*
parameter returns the true size of the data type, that is, the extent of
the corresponding typemap, ignoring MPI_LB and MPI_UB markers, and
performing no rounding for alignment. For both functions, if either the
*true_lb* or *true_extent* parameter cannot express the value to be
returned (e.g., if the parameter is too small to hold the output value),
it is set to MPI_UNDEFINED.

The *true_extent* is the minimum number of bytes of memory necessary to
hold a data type, uncompressed.

See section 4.1.8 of the MPI-3 standard for more detailed definitions of these
parameters in relation to the typemap.


ERRORS
------

.. include:: ./ERRORS.rst
