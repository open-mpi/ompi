.. _mpi_file_set_errhandler:


MPI_File_set_errhandler
=======================

.. include_body

:ref:`MPI_File_set_errhandler` |mdash| Sets the error handler for a file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_set_errhandler.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``file``: File (handle).

INPUT PARAMETER
---------------
* ``errhandler``: New error handler for file (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Attaches a new error handler to a file. The error handler must be either
a predefined error handler or an error handler created by a call to
:ref:`MPI_File_create_errhandler`.


ERRORS
------

.. include:: ./ERRORS.rst
