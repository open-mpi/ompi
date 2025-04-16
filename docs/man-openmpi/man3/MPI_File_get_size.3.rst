.. _mpi_file_get_size:


MPI_File_get_size
=================

.. include_body

:ref:`MPI_File_get_size` |mdash| Returns the current size of the file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_size.rst

INPUT PARAMETERS
----------------
* ``fh``: File handle (handle).
* ``size``: Size of the file in bytes (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_size` returns, in *size* , the current size in bytes of the
file associated with the file handle *fh*. Note that the file size
returned by Solaris may not represent the number of bytes physically
allocated for the file in those cases where all bytes in this file have
not been written at least once.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_File_preallocate`
