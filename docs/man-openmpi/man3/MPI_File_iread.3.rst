.. _mpi_file_iread:


MPI_File_iread
==============

.. include_body

:ref:`MPI_File_iread` |mdash| Reads a file starting at the location specified by
the individual file pointer (nonblocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_iread.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``count``: Number of elements in the buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iread` is a nonblocking version of :ref:`MPI_File_read`. It attempts to
read from the file associated with *fh* at the current individual file
pointer position maintained by the system in which a total number of
*count* data items having *datatype* type are read into the user's
buffer *buf.* The data is taken out of those parts of the file specified
by the current view. :ref:`MPI_File_iread` stores the number of data-type
elements actually read in *status.* All other fields of *status* are
undefined. It is erroneous to call this function if ``MPI_MODE_SEQUENTIAL``
mode was specified when the file was opened.


ERRORS
------

.. include:: ./ERRORS.rst
