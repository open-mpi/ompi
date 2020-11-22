.. _mpi_type_create_resized:


MPI_Type_create_resized
=======================

.. include_body

:ref:`MPI_Type_create_resized` - Returns a new data type with new extent
and upper and lower bounds.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb,
   	MPI_Aint extent, MPI_Datatype *newtype)


Fortran Syntax (see FORTRAN 77 NOTES)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
* ``IERROR``: Fortran only: Error status (integer).

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


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the *LB* and
*EXTENT* arguments only for Fortran 90. FORTRAN 77 users may use the
non-portable syntax

::

        INTEGER*MPI_ADDRESS_KIND LB
   or
        INTEGER*MPI_ADDRESS_KIND EXTENT

where MPI_ADDRESS_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.


NOTE
----

Use of :ref:`MPI_Type_create_resized` is strongly recommended over the old
MPI-1 functions :ref:`MPI_Type_extent` and :ref:`MPI_Type_lb`.


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
   :ref:`MPI_Type_get_extent`
