.. _mpi_type_get_extent:


MPI_Type_get_extent
===================

.. include_body

:ref:`MPI_Type_get_extent`, :ref:`MPI_Type_get_extent_x` - Returns the lower
bound and extent of a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_get_extent(MPI_Datatype datatype, MPI_Aint *lb,
   	MPI_Aint *extent)
   int MPI_Type_get_extent_x(MPI_Datatype datatype, MPI_Count *lb,
   	MPI_Count *extent)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_GET_EXTENT(DATATYPE, LB, EXTENT, IERROR)
   	INTEGER	DATATYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) LB, EXTENT
   MPI_TYPE_GET_EXTENT_X(DATATYPE, LB, EXTENT, IERROR)
   	INTEGER	DATATYPE, IERROR
   	INTEGER(KIND=MPI_COUNT_KIND) LB, EXTENT


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_get_extent(datatype, lb, extent, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: lb, extent
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   MPI_Type_get_extent_x(datatype, lb, extent, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER(KIND = MPI_COUNT_KIND), INTENT(OUT) :: lb, extent
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``datatype``: Data type (handle).

OUTPUT PARAMETERS
-----------------
* ``lb``: Lower bound of data type (integer).
* ``extent``: Data type extent (integer).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_get_extent` returns the lower bound and the extent of
*datatype*. For either function, if either the *lb* or *extent*
parameter cannot express the value to be returned (e.g., if the
parameter is too small to hold the output value), it is set to
MPI_UNDEFINED.


NOTE
----

Use of :ref:`MPI_Type_get_extent` is strongly recommended over the old MPI-1
functions :ref:`MPI_Type_extent` and :ref:`MPI_Type_lb`.


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the *LB* and
*EXTENT* arguments only for Fortran 90. FORTRAN 77 users may use the
non-portable syntax

:ref:`MPI_Type_get_extent`:

::

        INTEGER*MPI_ADDRESS_KIND LB
   or
        INTEGER*MPI_ADDRESS_KIND EXTENT

:ref:`MPI_Type_get_extent_x`:

::

        INTEGER*MPI_COUNT_KIND LB
   or
        INTEGER*MPI_COUNT_KIND EXTENT

where MPI_ADDRESS_KIND and MPI_COUNT_KIND are constants defined in
mpif.h and give the length of the declared integer in bytes.


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
