.. _mpi_remove_error_code:


MPI_Remove_error_code
=====================

.. include_body

:ref:`MPI_Remove_error_code` |mdash| Remove an error code associated with
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

Removes an error code associated with *errorcode*.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Remove_error_class`
   * :ref:`MPI_Error_class`
