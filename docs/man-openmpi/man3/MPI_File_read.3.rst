.. _mpi_file_read:


MPI_File_read
=============

.. include_body

:ref:`MPI_File_read` |mdash| Reads a file starting at the location specified by
the individual file pointer (blocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_read.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (integer).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read` attempts to read from the file associated with *fh* (at
the current individual file pointer position maintained by the system) a
total number of *count* data items having *datatype* type into the
user's buffer *buf.* The data is taken out of those parts of the file
specified by the current view. :ref:`MPI_File_read` stores the number of
data-type elements actually read in *status.* All other fields of
*status* are undefined.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
