.. _mpi_file_read_all:


MPI_File_read_all
=================

.. include_body

:ref:`MPI_File_read_all` |mdash| Reads a file starting at the locations specified
by individual file pointers (blocking, collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_read_all.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_all` is a collective routine that attempts to read from the
file associated with *fh* (at the current individual file pointer
position maintained by the system) a total number of *count* data items
having *datatype* type into the user's buffer *buf.* The data is taken
out of those parts of the file specified by the current view.
:ref:`MPI_File_read_all` stores the number of data-type elements actually read
in *status.* All other fields of *status* are undefined.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
