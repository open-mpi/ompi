.. _mpi_file_write_shared:

MPI_File_write_shared
=====================

.. include_body

:ref:`MPI_File_write_shared` |mdash| Writes a file using the shared file pointer
(blocking, noncollective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_write_shared.rst

INPUT/OUTPUT PARAMETER
----------------------

* ``fh`` : File handle (handle).

INPUT PARAMETERS
----------------

* ``buf`` : Initial address of buffer (choice).
* ``count`` : Number of elements in buffer (integer).
* ``datatype`` : Data type of each buffer element (handle).

OUTPUT PARAMETERS
-----------------

* ``status`` : Status object (status).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_write_shared` is a blocking routine that uses the shared
file pointer to write files. The order of serialization is not
deterministic for this noncollective routine.

ERRORS
------

.. include:: ./ERRORS.rst
