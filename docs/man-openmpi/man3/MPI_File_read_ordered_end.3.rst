.. _mpi_file_read_ordered_end:


MPI_File_read_ordered_end
=========================

.. include_body

:ref:`MPI_File_read_ordered_end` |mdash| Reads a file at a location specified by
a shared file pointer; ending part of a split collective routine
(blocking).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_read_ordered_end.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_ordered_end` is the ending part of a split collective
routine that must be called by all processes in the communicator group
associated with the file handle *fh.* MPI_File_rad_ordered_end blocks
until the operation initiated by :ref:`MPI_File_read_ordered_begin` completes.
It attempts to read the file associated with *fh* into the user's buffer
*buf.* The shared file pointer is updated by the amounts of data
requested by all processes of the group. For each process, the location
in the file at which data is read is the position at which the shared
file pointer would be after all processes whose ranks within the group
are less than that of this process had read their data.


NOTES
-----

All the nonblocking collective routines for data access are "split" into
two routines, each with ``_begin`` or ``_end`` as a suffix. These split
collective routines are subject to the semantic rules described in
Section 9.4.5 of the MPI-2 standard.


ERRORS
------

.. include:: ./ERRORS.rst
