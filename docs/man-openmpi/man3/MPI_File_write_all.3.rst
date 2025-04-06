.. _mpi_file_write_all:


MPI_File_write_all
==================

.. include_body

:ref:`MPI_File_write_all` |mdash| Writes a file starting at the locations
specified by individual file pointers (blocking, collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_write_all.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``buf``: Initial address of buffer (choice).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write_all` is a collective routine that attempts to write into
the file associated with *fh* (at the current individual file pointer
position maintained by the system) a total number of *count* data items
having *datatype* type from the user's buffer *buf.* The data is written
into those parts of the file specified by the current view.
:ref:`MPI_File_write_all` stores the number of *datatype* elements actually
written in *status.* All other fields of *status* are undefined.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
