.. _mpi_file_get_group:


MPI_File_get_group
==================

.. include_body

:ref:`MPI_File_get_group` |mdash| Returns a duplicate of the process group of a
file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_group.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``group``: Group that opened the file (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_File_get_group` returns a duplicate of the group of the communicator
used to open the file associated with *fh.* The group is returned in
*group.* The user is responsible for freeing *group,* using
:ref:`MPI_Group_free`.


ERRORS
------

.. include:: ./ERRORS.rst
