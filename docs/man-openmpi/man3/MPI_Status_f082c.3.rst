.. _mpi_status_f082c:

MPI_Status_f082c
================

.. include_body

:ref:`MPI_Status_f082c`, :ref:`MPI_Status_c2f08` - Translates a C status into a
Fortran 2008 status, or vice versa.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Status_f082c(const MPI_F08_status *f08_status, MPI_Status *c_status)
   int MPI_Status_c2f08(const MPI_Status *c_status, MPI_F08_status *f08_status)

PARAMETERS
----------

* ``f08_status`` : mpi_f08-style MPI status object
* ``c_status`` : C-style MPI status object

DESCRIPTION
-----------

These two procedures are provided in C to convert from a Fortran 2008
status (which is a derived type made of integers) to a C status (which
is a structure), and vice versa. The conversion occurs on all the
information in status, including that which is hidden. That is, no
status information is lost in the conversion.

When using :ref:`MPI_Status_f082c`, if ``f08_status`` is a valid Fortran status,
but not the Fortran value of ``MPI_F08_STATUS_IGNORE`` or
``MPI_F08_STATUSES_IGNORE``, then :ref:`MPI_Status_f082c` returns in ``c_status`` a
valid C status with the same content. If ``f08_status`` is the Fortran value
of ``MPI_STATUS_IGNORE`` or ``MPI_STATUSES_IGNORE``, or if ``f08_status`` is not a
valid Fortran status, then the call is erroneous.

When using :ref:`MPI_Status_c2f08`, the opposite conversion is applied. If
``c_status`` is ``MPI_STATUS_IGNORE`` or ``MPI_STATUSES_IGNORE``, or if ``c_status`` is
not a valid C status, then the call is erroneous.

The input status has the same source, tag and error code values as the
output status, and returns the same answers when queried for count,
elements, and cancellation. The conversion function may be called with
an input status argument that has an undefined error field, in which
case the value of the error field in the output status argument is
undefined.

NOTES
-----

These functions are only available in C; they are not available in any
of the Fortran MPI interfaces.


.. seealso:: :ref:`MPI_Status_c2f` :ref:`MPI_Status_f2f08`
