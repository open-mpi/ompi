.. _mpi_remove_error_class:


MPI_Remove_error_class
======================

.. include_body

:ref:`MPI_Remove_error_class` |mdash| Removes an error class.

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

The function :ref:`MPI_Remove_error_class` removes a local error class.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Remove_error_code`
   * :ref:`MPI_Remove_error_string`
   * :ref:`MPI_Error_class`
   * :ref:`MPI_Error_string`
