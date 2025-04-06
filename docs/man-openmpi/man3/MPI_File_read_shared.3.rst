.. _mpi_file_read_shared:


MPI_File_read_shared
====================

.. include_body

:ref:`MPI_File_read_shared` |mdash| Reads a file using the shared file pointer
(blocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_read_shared.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``count``: Number of elements in buffer (integer)
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``status``: Status object (status).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_read_shared` is a blocking routine that uses the shared file
pointer to read files. The order of serialization is not deterministic
for this noncollective routine.


ERRORS
------

.. include:: ./ERRORS.rst
