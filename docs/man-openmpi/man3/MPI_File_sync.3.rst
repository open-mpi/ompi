.. _mpi_file_sync:


MPI_File_sync
=============

.. include_body

:ref:`MPI_File_sync` |mdash| Makes semantics consistent for data-access
operations (collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_sync.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Calling :ref:`MPI_File_sync` with *fh* causes all previous writes to *fh* by
the calling process to be written to permanent storage. If other
processes have made updates to permanent storage, then all such updates
become visible to subsequent reads of *fh* by the calling process.

:ref:`MPI_File_sync` is a collective operation. The user is responsible for
ensuring that all nonblocking requests on *fh* have been completed
before calling :ref:`MPI_File_sync`. Otherwise, the call to :ref:`MPI_File_sync` is
erroneous.


ERRORS
------

.. include:: ./ERRORS.rst
