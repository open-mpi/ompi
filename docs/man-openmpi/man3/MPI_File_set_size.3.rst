.. _mpi_file_set_size:


MPI_File_set_size
=================

.. include_body

:ref:`MPI_File_set_size` |mdash| Resizes a file (collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_set_size.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``size``: Size to truncate or expand file (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_set_size` resizes the file associated with the file handle *fh,*
truncating UNIX files as necessary. :ref:`MPI_File_set_size` is collective; all
processes in the group must pass identical values for size.

When using :ref:`MPI_File_set_size` on a UNIX file, if *size* is larger than
the current file size, the file size becomes *size*. If *size* is
smaller than the current file size, the file is truncated at the
position defined by *size* (from the beginning of the file and measured
in bytes). Regions of the file which have been previously written are
unaffected.

:ref:`MPI_File_set_size` does not affect the individual file pointers or the
shared file pointer.

Note that the actual amount of storage space cannot be allocated by
:ref:`MPI_File_set_size`. Use :ref:`MPI_File_preallocate` to accomplish this.

It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL`` mode was
specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
