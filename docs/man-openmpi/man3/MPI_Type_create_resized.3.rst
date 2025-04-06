.. _mpi_type_create_resized:


MPI_Type_create_resized
=======================

.. include_body

:ref:`MPI_Type_create_resized` |mdash| Returns a new data type with new extent
and upper and lower bounds.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_create_resized.rst

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
