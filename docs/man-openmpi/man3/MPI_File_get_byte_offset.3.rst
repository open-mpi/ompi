.. _mpi_file_get_byte_offset:


MPI_File_get_byte_offset
========================

.. include_body

:ref:`MPI_File_get_byte_offset` |mdash| Converts a view-relative offset into an
absolute byte position.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_byte_offset.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``offset``: Offset (integer).

OUTPUT PARAMETERS
-----------------
* ``disp``: Absolute byte position of offset (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_byte_offset` converts an offset specified for the current
view to its corresponding displacement value, or absolute byte position,
from the beginning of the file. The absolute byte position of *offset*
relative to the current view of *fh* is returned in *disp*.


ERRORS
------

.. include:: ./ERRORS.rst
