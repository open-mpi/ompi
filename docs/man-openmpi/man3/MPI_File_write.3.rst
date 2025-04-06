.. _mpi_file_write:


MPI_File_write
==============

.. include_body

:ref:`MPI_File_write` |mdash| Writes a file starting at the location specified by
the individual file pointer (blocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_write.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``buf``: Initial address of buffer (choice).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write` attempts to write into the file associated with *fh* (at
the current individual file pointer position maintained by the system) a
total number of *count* data items having *datatype* type from the
user's buffer *buf.* The data is written into those parts of the file
specified by the current view. :ref:`MPI_File_write` stores the number of
*datatype* elements actually written in *status.* All other fields of
*status* are undefined.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
