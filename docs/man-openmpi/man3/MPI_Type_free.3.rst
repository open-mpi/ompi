.. _mpi_type_free:


MPI_Type_free
=============

.. include_body

:ref:`MPI_Type_free` - Frees a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_free(MPI_Datatype *datatype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_FREE(DATATYPE, IERROR)
   	INTEGER	DATATYPE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_free(datatype, ierror)
   	TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``datatype``: Datatype that is freed (handle).

OUTPUT PARAMETER
----------------
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Marks the datatype object associated with datatype for de-allocation and
sets datatype to MPI_DATATYPE_NULL. Any communication that is currently
using this datatype will complete normally. Derived datatypes that were
defined from the freed datatype are not affected.

Freeing a datatype does not affect any other datatype that was built
from the freed datatype. The system behaves as if input datatype
arguments to derived datatype constructors are passed by value.


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
