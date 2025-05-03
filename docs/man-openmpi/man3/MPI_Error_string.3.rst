.. _mpi_error_string:


MPI_Error_string
================

.. include_body

:ref:`MPI_Error_string` |mdash| Returns a string for a given error code.

.. The following file was automatically generated
.. include:: ./bindings/mpi_error_string.rst

INPUT PARAMETER
---------------
* ``errorcode``: Error code returned by an MPI routine or an MPI error class.

OUTPUT PARAMETERS
-----------------
* ``string``: Text that corresponds to the errorcode.
* ``resultlen``: Length of string.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns the error string associated with an error code or class. The
argument string must represent storage that is at least
MPI_MAX_ERROR_STRING characters long.

The number of characters actually written is returned in the output
argument, resultlen.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Error_class`
