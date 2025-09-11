.. _mpi_remove_error_string:


MPI_Remove_error_string
=======================

.. include_body

:ref:`MPI_Remove_error_string` |mdash| Removes the error string associated with
a user-created error code.

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

This routine removes an error string associated with a user-created error code.
It is erroneous to call :ref:`MPI_Remove_error_string` with a value for *errorcode*
that does not have an error string added by a call to :ref:`MPI_Add_error_string`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Add_error_string`
   * :ref:`MPI_Remove_error_class`
   * :ref:`MPI_Remove_error_code`
   * :ref:`MPI_Error_class`
   * :ref:`MPI_Error_string`
