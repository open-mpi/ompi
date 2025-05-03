.. _mpi_file_read_at_all_end:


MPI_File_read_at_all_end
========================

.. include_body

:ref:`MPI_File_read_at_all_end` |mdash| Reads a file at explicitly specified
offsets; ending part of a split collective routine (blocking).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_read_at_all_end.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_at_all_end` is a split collective routine that stores the
number of elements actually read from the file associated with *fh* in
*status.* :ref:`MPI_File_read_at_all_end` blocks until the operation initiated
by :ref:`MPI_File_read_at_all_begin` completes. The data is taken out of those
parts of the file specified by the current view. All other fields of
*status* are undefined.


NOTES
-----

All the nonblocking collective routines for data access are "split" into
two routines, each with ``_begin`` or ``_end`` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
