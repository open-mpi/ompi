.. _mpi_remove_error_class:


MPI_Remove_error_class
======================

.. include_body

:ref:`MPI_Remove_error_class` |mdash| Removes a user-created error class.

.. The following file was automatically generated
.. include:: ./bindings/mpi_remove_error_class.rst

INPUT PARAMETERS
----------------
* ``errorclass``: New error class (integer).

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Remove_error_class` removes a user-created error class.
It is erroneous to call :ref:`MPI_Remove_error_class` with a value for
*errorclass* that was not added by a call to :ref:`MPI_Add_error_class`.
It is erroneous to remove an error class when its associated error codes
have not been removed before.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Add_error_class`
   * :ref:`MPI_Remove_error_code`
   * :ref:`MPI_Remove_error_string`
   * :ref:`MPI_Error_class`
   * :ref:`MPI_Error_string`
