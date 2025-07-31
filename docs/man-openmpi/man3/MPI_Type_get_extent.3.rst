.. _mpi_type_get_extent:


MPI_Type_get_extent
===================

.. include_body

:ref:`MPI_Type_get_extent`, :ref:`MPI_Type_get_extent_x` - Returns the lower
bound and extent of a data type.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Type_get_extent, MPI_Type_get_extent_x

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_get_extent.rst

INPUT PARAMETER
---------------
* ``datatype``: Data type (handle).

OUTPUT PARAMETERS
-----------------
* ``lb``: Lower bound of data type (integer).
* ``extent``: Data type extent (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_get_extent` returns the lower bound and the extent of
*datatype*. For either function, if either the *lb* or *extent*
parameter cannot express the value to be returned (e.g., if the
parameter is too small to hold the output value), it is set to
MPI_UNDEFINED.


NOTES
-----

Use of :ref:`MPI_Type_get_extent` is strongly recommended over the legacy MPI-1
functions :ref:`MPI_Type_extent` and :ref:`MPI_Type_lb`.

Note that :ref:`MPI_Type_get_extent_x` is  *deprecated* as of MPI-4.1. Please use
the big count version of :ref:`MPI_Type_get_extent` instead.


ERRORS
------

.. include:: ./ERRORS.rst
