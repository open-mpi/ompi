.. _mpi_file_read_at_all_begin:


MPI_File_read_at_all_begin
==========================

.. include_body

:ref:`MPI_File_read_at_all_begin` |mdash| Reads a file at explicitly specified
offsets; beginning part of a split collective routine (nonblocking).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_read_at_all_begin.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: File offset (integer).
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element.

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_at_all_begin` is the beginning part of a split collective
routine that attempts to read from the file associated with *fh* (at the
*offset* position) a total number of *count* data items having
*datatype* type into the user's buffer *buf.* The *offset* is in etype
units relative to the current view. That is, holes are not counted when
locating an offset. The data is taken out of those parts of the file
specified by the current view.


NOTES
-----

All the nonblocking collective routines for data access are "split" into
two routines, each with ``_begin`` or ``_end`` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
