.. _mpi_remove_error_code:


MPI_Remove_error_code
=====================

.. include_body

:ref:`MPI_Remove_error_code` |mdash| Remove a user-created error code associated with
*errorcode*

.. The following file was automatically generated
.. include:: ./bindings/mpi_remove_error_code.rst

INPUT PARAMETER
---------------
* ``errorcode``: MPI error code (integer).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Removes a user-created error code associated with *errorcode*.
It is erroneous to call :ref:`MPI_Remove_error_code` with a value for
*errorcode* that was not added by a call to :ref:`MPI_Add_error_code`.
It is erroneous to remove an error code when its associated error string has
not been removed before.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Add_error_code`
   * :ref:`MPI_Remove_error_class`
   * :ref:`MPI_Remove_error_string`
   * :ref:`MPI_Error_class`
