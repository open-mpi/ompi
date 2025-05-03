.. _mpi_file_close:


MPI_File_close
==============

.. include_body

:ref:`MPI_File_close` |mdash| Closes a file (collective).

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_close.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``fh``: File handle (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_close` first synchronizes file state, then closes the file
associated with *fh.* :ref:`MPI_File_close` is a collective routine. The user
is responsible for ensuring that all outstanding requests associated
with *fh* have completed before calling :ref:`MPI_File_close`.


ERRORS
------

.. include:: ./ERRORS.rst
