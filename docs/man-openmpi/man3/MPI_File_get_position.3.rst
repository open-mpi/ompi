.. _mpi_file_get_position:


MPI_File_get_position
=====================

.. include_body

:ref:`MPI_File_get_position` |mdash| Returns the current position of the
individual file pointer.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_position.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``offset``: Offset of the individual file pointer (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_position` returns, in *offset,* the current position of the
individual file pointer in *etype* units relative to the current
displacement and file type.


ERRORS
------

.. include:: ./ERRORS.rst
