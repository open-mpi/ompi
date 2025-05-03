.. _mpi_file_delete:


MPI_File_delete
===============

.. include_body

:ref:`MPI_File_delete` |mdash| Deletes a file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_delete.rst

INPUT PARAMETERS
----------------
* ``filename``: Name of file to delete (string).
* ``info``: Info object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_delete` deletes the file identified by the file name *filename*,
provided it is not currently open by any process. It is an error to
delete the file with :ref:`MPI_File_delete` if some process has it open, but
:ref:`MPI_File_delete` does not check this. If the file does not exist,
:ref:`MPI_File_delete` returns an error in the class MPI_ERR_NO_SUCH_FILE.


ERRORS
------

.. include:: ./ERRORS.rst
