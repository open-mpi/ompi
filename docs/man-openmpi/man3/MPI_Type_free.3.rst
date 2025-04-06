.. _mpi_type_free:


MPI_Type_free
=============

.. include_body

:ref:`MPI_Type_free` |mdash| Frees a data type.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_free.rst

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
