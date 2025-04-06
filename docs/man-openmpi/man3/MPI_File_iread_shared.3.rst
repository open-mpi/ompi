.. _mpi_file_iread_shared:


MPI_File_iread_shared
=====================

.. include_body

:ref:`MPI_File_iread_shared` |mdash| Reads a file using the shared file pointer
(nonblocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_iread_shared.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

INPUT PARAMETERS
----------------
* ``count``: Number of elements in buffer (integer).
* ``datatype``: Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------
* ``buf``: Initial address of buffer (choice).
* ``request``: Request object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_iread_shared` is a nonblocking version of the
:ref:`MPI_File_read_shared` interface. It uses the shared file pointer to read
files. The order of serialization among the processors is not
deterministic for this noncollective routine, so you need to use other
methods of synchronization to impose a particular order among
processors.


ERRORS
------

.. include:: ./ERRORS.rst
