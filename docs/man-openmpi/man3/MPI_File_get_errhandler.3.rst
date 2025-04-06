.. _mpi_file_get_errhandler:


MPI_File_get_errhandler
=======================

.. include_body

:ref:`MPI_File_get_errhandler` |mdash| Gets the error handler for a file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_errhandler.rst

INPUT PARAMETER
---------------
* ``file``: File (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: MPI error handler currently associated with file (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Returns in *errhandler* (a handle to) the error handler that is
currently associated with file *file*.


ERRORS
------

.. include:: ./ERRORS.rst
