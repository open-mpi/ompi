.. _mpi_file_iwrite_shared:


MPI_File_iwrite_shared
======================

.. include_body

:ref:`MPI_File_iwrite_shared` |mdash| Writes a file using the shared file pointer
(nonblocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_iwrite_shared.rst

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

:ref:`MPI_File_iwrite_shared` is a nonblocking routine that uses the shared
file pointer to write files. The order of serialization is not
deterministic for this noncollective routine, so you need to use other
methods of synchronization to impose a particular order.


ERRORS
------

.. include:: ./ERRORS.rst
