.. _mpi_file_create_errhandler:


MPI_File_create_errhandler
==========================

.. include_body

:ref:`MPI_File_create_errhandler` |mdash| Creates an MPI-style error handler that
can be attached to a file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_create_errhandler.rst

DEPRECATED TYPE NAME NOTE
-------------------------

MPI-2.2 deprecated the ``MPI_File_errhandler_fn`` type in favor of
``MPI_File_errhandler_function``. Open MPI supports both
names (indeed, the ``_fn`` names are typedefs to the ``_function`` names).


INPUT PARAMETER
---------------

* ``function``: User-defined error handling procedure (function).

OUTPUT PARAMETERS
-----------------

* ``errhandler``: MPI error handler (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_create_errhandler` creates an error handler that can be
attached to file operations. In C, the
user routine should be a ``function`` of type
``MPI_File_errhandler_function``, which is defined as

.. code-block:: c

       typedef void (MPI_File_errhandler_function)(MPI_File *, int *, ...);

The first argument is the file in use. The second is the
error code to be returned by the MPI routine that raised the error.

In the Fortran language, the user routine should be of the form:

.. code-block:: fortran

       SUBROUTINE FILE_ERRHANDLER_FUNCTION(FILE, ERROR_CODE, ...)
           INTEGER FILE, ERROR_CODE


ERRORS
------

.. include:: ./ERRORS.rst
