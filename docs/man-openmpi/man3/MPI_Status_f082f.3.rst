.. _mpi_status_f082f:

MPI_Status_f082f
================

.. include_body

:ref:`MPI_Status_f082f`, :ref:`MPI_Status_f2f08` - Translates a Fortran 2008 status
into a Fortran INTEGER-style status, or vice versa.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Status_f082f, MPI_Status_f2f08

.. The following file was automatically generated
.. include:: ./bindings/mpi_status_f082f.rst

PARAMETERS
----------

* ``f08_status`` : mpi_f08-style MPI status object
* ``f_status`` : mpi-style INTEGER MPI status object

DESCRIPTION
-----------

These two procedures are provided to convert from a Fortran 2008 status
(which is a derived datatype made of integers) to a Fortran status
(which is an array of integers), and vice versa. The conversion occurs
on all the information in status, including that which is hidden. That
is, no status information is lost in the conversion.

When using :ref:`MPI_Status_f082f`, if ``f08_status`` is a valid Fortran status,
but not the Fortran value of ``MPI_F08_STATUS_IGNORE`` (in C),
``MPI_STATUS_IGNORE`` (in Fortran) or ``MPI_F08_STATUSES_IGNORE`` (in C) or
``MPI_STATUSES_IGNORE`` (in Fortran), then :ref:`MPI_Status_f082f` returns in
``f_status`` a valid array with the same content. If ``f08_status`` is the C
value of ``MPI_F08_STATUS_IGNORE`` or ``MPI_F08_STATUSES_IGNORE`` or the Fortran
value of ``MPI_STATUS_IGNORE`` or ``MPI_STATUSES_IGNORE``, or if ``f08_status`` is
not a valid Fortran status, then the call is erroneous.

When using :ref:`MPI_Status_f2f08`, the opposite conversion is applied. If
``f_status`` is ``MPI_STATUS_IGNORE`` or ``MPI_STATUSES_IGNORE``, or if ``f_status`` is
not a valid Fortran status, then the call is erroneous.

The input status has the same source, tag and error code values as the
output status, and returns the same answers when queried for count,
elements, and cancellation. The conversion function may be called with
an input status argument that has an undefined error field, in which
case the value of the error field in the output status argument is
undefined.

NOTES
-----

The Fortran subroutines for these MPI routines are only available in the
mpi and mpi_f08 modules (including the type specification for
``TYPE(MPI_Status)``; they are (intentionally) not available in ``mpif.h``.


.. seealso:: :ref:`MPI_Status_c2f` :ref:`MPI_Status_c2f08`
