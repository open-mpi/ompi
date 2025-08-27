.. _mpi_remove_error_string:


MPI_Remove_error_string
=======================

.. include_body

:ref:`MPI_Remove_error_string` |mdash| Removes the error string associated with
an error code.

.. The following file was automatically generated
.. include:: ./bindings/mpi_remove_error_string.rst

INPUT PARAMETERS
----------------
* ``errorcode``: MPI error code, returned by an MPI routine (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine removes an error string associated with an error code.
It is erroneous to call :ref:`MPI_Remove_error_string` for an error value not
generated via :ref:`MPI_Remove_error_code` (e.g., an error
code with a value not greater than MPI_LAST_ERRCODE).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Remove_error_class`
   * :ref:`MPI_Remove_error_code`
   * :ref:`MPI_Error_class`
   * :ref:`MPI_Error_string`
