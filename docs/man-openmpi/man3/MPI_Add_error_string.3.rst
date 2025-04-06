.. _mpi_add_error_string:


MPI_Add_error_string
====================

.. include_body

:ref:`MPI_Add_error_string` |mdash| Associates a string with an error code or class

.. The following file was automatically generated
.. include:: ./bindings/mpi_add_error_string.rst

INPUT PARAMETERS
----------------
* ``errorcode``: MPI error class, or an error code returned by an MPI routine (integer).
* ``string``: Text that corresponds to the error code or class (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine associates an error string with an error code or class.
Calling :ref:`MPI_Add_error_string` for an error code or class that already has
an associated error string will replace the old string with the new one.
It is erroneous to call :ref:`MPI_Add_error_string` for an error value not
generated via :ref:`MPI_Add_error_class` or :ref:`MPI_Add_error_code` (e.g., an error
code or class with a value not greater than MPI_LAST_ERRCODE).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Add_error_class`
   * :ref:`MPI_Add_error_code`
   * :ref:`MPI_Error_class`
   * :ref:`MPI_Error_string`
