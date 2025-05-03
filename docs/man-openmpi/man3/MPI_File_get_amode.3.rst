.. _mpi_file_get_amode:


MPI_File_get_amode
==================

.. include_body

:ref:`MPI_File_get_amode` |mdash| Returns access mode associated with an open
file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_amode.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``amode``: File access mode used to open the file (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_amode` returns, in *amode,* the access mode associated with
the open file *fh.*


ERRORS
------

.. include:: ./ERRORS.rst
