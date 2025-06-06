.. _mpi_file_write_all_begin:


MPI_File_write_all_begin
========================

.. include_body

:ref:`MPI_File_write_all_begin` |mdash| Writes a file starting at the locations
specified by individual file pointers; beginning part of a split
collective routine (nonblocking).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_write_all_begin.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``buf``: Initial address of buffer (choice).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write_all_begin` is the beginning part of a split collective,
nonblocking routine that attempts to write into the file associated with
*fh* (at the current individual file pointer position maintained by the
system) a total number of *count* data items having *datatype* type from
the user's buffer *buf.* The data is written into those parts of the
file specified by the current view.


NOTES
-----

All the nonblocking collective routines for data access are "split" into
two routines, each with ``_begin`` or ``_end`` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
