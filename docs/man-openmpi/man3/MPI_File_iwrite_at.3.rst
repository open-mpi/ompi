.. _mpi_file_iwrite_at:


MPI_File_iwrite_at
==================

.. include_body

:ref:`MPI_File_iwrite_at` |mdash| Writes a file at an explicitly specified offset
(nonblocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_iwrite_at.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``offset``: File offset (integer).
* ``buf``: Initial address of buffer (choice).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iwrite_at` is a nonblocking version of :ref:`MPI_File_write_at`. It
attempts to write into the file associated with *fh* (at the *offset*
position) a total number of *count* data items having *datatype* type
from the user's buffer *buf.* The offset is in *etype* units relative to
the current view. That is, holes are not counted when locating an
offset. The data is written into those parts of the file specified by
the current view. :ref:`MPI_File_iwrite_at` stores the number of *datatype*
elements actually written in *status.* All other fields of *status* are
undefined. The request structure can be passed to :ref:`MPI_Wait` or :ref:`MPI_Test`,
which will return a status with the number of bytes actually accessed.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was open.


ERRORS
------

.. include:: ./ERRORS.rst
