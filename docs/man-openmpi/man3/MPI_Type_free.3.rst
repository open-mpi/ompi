.. _mpi_type_free:


MPI_Type_free
=============

.. include_body

:ref:`MPI_Type_free` |mdash| Frees a data type.


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
* ``ierror``: Fortran only: Error status (integer).

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

.. include:: ./ERRORS.rst
