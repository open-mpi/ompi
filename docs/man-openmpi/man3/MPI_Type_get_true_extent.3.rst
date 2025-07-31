.. _mpi_type_get_true_extent:


MPI_Type_get_true_extent
========================

.. include_body

:ref:`MPI_Type_get_true_extent`, :ref:`MPI_Type_get_true_extent_x` - Returns
the true lower bound and extent of a data type's corresponding typemap,
ignoring MPI_UB and MPI_LB markers.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Type_get_true_extent, MPI_Type_get_true_extent_x

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_get_true_extent.rst

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

NOTE
----

Note that :ref:`MPI_Type_get_true_extent_x` is  *deprecated* as of MPI-4.1. Please use
the big count version of :ref:`MPI_Type_get_true_extent` instead.

ERRORS
------

.. include:: ./ERRORS.rst
