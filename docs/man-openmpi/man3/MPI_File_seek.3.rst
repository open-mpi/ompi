.. _mpi_file_seek:


MPI_File_seek
=============

.. include_body

:ref:`MPI_File_seek` |mdash| Updates individual file pointers (noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_seek.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: File offset (integer).
* ``whence``: Update mode (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_seek` updates the individual file pointer according to *whence,*
which could have the following possible values:

* ``MPI_SEEK_SET`` - The pointer is set to *offset.*
* ``MPI_SEEK_CUR`` - The pointer is set to the current pointer position plus *offset.*
* ``MPI_SEEK_END`` - The pointer is set to the end of the file plus *offset.*

The *offset* can be negative, which allows seeking backwards. It is
erroneous to seek to a negative position in the file. The end of the
file is defined to be the location of the next elementary data item
immediately after the last accessed data item, even if that location is
a hole.


ERRORS
------

.. include:: ./ERRORS.rst
