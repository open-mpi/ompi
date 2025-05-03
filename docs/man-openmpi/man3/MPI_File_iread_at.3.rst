.. _mpi_file_iread_at:


MPI_File_iread_at
=================

.. include_body

:ref:`MPI_File_iread_at` |mdash| Reads a file at an explicitly specified offset
(nonblocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_iread_at.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: File offset (integer).
* ``count``: Number of elements in the buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of the buffer (choice).
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iread_at` is the nonblocking version of :ref:`MPI_File_read_at`.

:ref:`MPI_File_iread_at` is a nonblocking routine that attempts to read from
the file associated with *fh* at the *offset* position a total number of
*count* data items having *datatype* type into the user's buffer *buf.*
The *offset* is in etype units relative to the current view. That is,
holes are not counted when locating an offset. The data is taken out of
those parts of the file specified by the current view. :ref:`MPI_File_iread_at`
stores the number of *datatype* elements actually read in *status.* All
other fields of *status* are undefined.


ERRORS
------

.. include:: ./ERRORS.rst
