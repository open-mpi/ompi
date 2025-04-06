.. _mpi_file_get_atomicity:


MPI_File_get_atomicity
======================

.. include_body

:ref:`MPI_File_get_atomicity` |mdash| Returns current consistency semantics for
data-access operations.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_atomicity.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETER
----------------
* ``flag``: true if atomic mode is enabled, false if nonatomic mode is enabled (boolean).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_atomicity` returns the current consistency semantics for
data access operations on the set of file handles created by one
collective :ref:`MPI_File_open`. If *flag* is *true,* atomic mode is currently
enabled; if *flag* is *false,* nonatomic mode is currently enabled.


ERRORS
------

.. include:: ./ERRORS.rst
