.. _mpi_error_class:


MPI_Error_class
===============

.. include_body

:ref:`MPI_Error_class` |mdash| Converts an error code into an error class.

.. The following file was automatically generated
.. include:: ./bindings/mpi_error_class.rst

INPUT PARAMETER
---------------
* ``errorcode``: Error code returned by an MPI routine.

OUTPUT PARAMETERS
-----------------
* ``errorclass``: Error class associated with errorcode.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Error_class` maps each standard error code (error class)
onto itself.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Error_string`
