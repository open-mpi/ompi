.. _mpi_type_size:


MPI_Type_size
=============

.. include_body

:ref:`MPI_Type_size`, :ref:`MPI_Type_size_x` - Returns the number of bytes
occupied by entries in a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_size(MPI_Datatype datatype, int *size)
   int MPI_Type_size_x(MPI_Datatype datatype, MPI_Count *size)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_SIZE(DATATYPE, SIZE, IERROR)
   	INTEGER	DATATYPE, SIZE, IERROR
   MPI_TYPE_SIZE_X(DATATYPE, SIZE, IERROR)
   	INTEGER	DATATYPE
           INTEGER(KIND=MPI_COUNT_KIND) SIZE
           INTEGER IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_size(datatype, size, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   MPI_Type_size_x(datatype, size, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER(KIND=MPI_COUNT_KIND), INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``datatype``: Datatype (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Datatype size (integer).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_size` returns the total size, in bytes, of the entries in the
type signature associated with datatype; i.e., the total size of the
data in a message that would be created with this datatype. Entries that
occur multiple times in the datatype are counted with their
multiplicity. For either function, if the *size* parameter cannot
express the value to be returned (e.g., if the parameter is too small to
hold the output value), it is set to MPI_UNDEFINED.


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


FORTRAN 77 NOTES
----------------

The MPI standard prescribes portable Fortran syntax for the *SIZE*
argument of :ref:`MPI_Type_size_x` only for Fortran 90. FORTRAN 77 users may
use the non-portable syntax

::

        INTEGER*MPI_COUNT_KIND SIZE

where MPI_COUNT_KIND is a constant defined in mpif.h and gives the
length of the declared integer in bytes.
