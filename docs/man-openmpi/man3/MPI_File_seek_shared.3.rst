.. _mpi_file_seek_shared:


MPI_File_seek_shared
====================

.. include_body

:ref:`MPI_File_seek_shared` |mdash| Updates the global shared file pointer
(collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_seek_shared.rst

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

:ref:`MPI_File_seek_shared` updates the shared file pointer according to
*whence,* which could have the following possible values:

* ``MPI_SEEK_SET`` - The pointer is set to *offset.*
* ``MPI_SEEK_CUR`` - The pointer is set to the current pointer position plus *offset.*
* ``MPI_SEEK_END`` - The pointer is set to the end of the file plus *offset.*

:ref:`MPI_File_seek_shared` is collective; all the processes in the
communicator group associated with the file handle *fh* must call
:ref:`MPI_File_seek_shared` with the same *offset* and *whence.* All processes
in the communicator group are synchronized before the shared file
pointer is updated.

The *offset* can be negative, which allows seeking backwards. It is
erroneous to seek to a negative position in the view. The end of the
view is defined to be the position of the next elementary data item,
relative to the current view, following the last whole elementary data
item accessible.


ERRORS
------

.. include:: ./ERRORS.rst
