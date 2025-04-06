.. _mpi_file_write_at:


MPI_File_write_at
=================

.. include_body

:ref:`MPI_File_write_at` |mdash| Writes a file at an explicitly specified offset
(blocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_write_at.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: File offset (integer).
* ``buf``: Initial address of buffer (choice).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write_at` attempts to write into the file associated with *fh*
(at the *offset* position) a total number of *count* data items having
*datatype* type from the user's buffer *buf.* The offset is in *etype*
units relative to the current view. That is, holes are not counted when
locating an offset. The data is written into those parts of the file
specified by the current view. :ref:`MPI_File_write_at` stores the number of
*datatype* elements actually written in *status.* All other fields of
*status* are undefined.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_File_iwrite_at`
   * :ref:`MPI_File_write_at_all`
   * :ref:`MPI_File_write_at_all_begin`
   * :ref:`MPI_File_write_at_all_end`
