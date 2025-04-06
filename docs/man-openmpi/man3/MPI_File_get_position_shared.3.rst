.. _mpi_file_get_position_shared:


MPI_File_get_position_shared
============================

.. include_body

:ref:`MPI_File_get_position_shared` |mdash| Returns the current position of the
shared file pointer.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_position_shared.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``offset``: Offset of the shared file pointer (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_position_shared` returns, in *offset,* the current position
of the shared file pointer in *etype* units relative to the current
displacement and file type.


ERRORS
------

.. include:: ./ERRORS.rst
