.. _mpi_file_get_view:


MPI_File_get_view
=================

.. include_body

:ref:`MPI_File_get_view` |mdash| Returns the process's view of data in the file.

.. The following file was automatically generated
.. include:: ./bindings/mpi_file_get_view.rst

INPUT PARAMETER
---------------
* ``fh``: File handle (handle).

OUTPUT PARAMETERS
-----------------
* ``disp``: Displacement (integer).
* ``etype``: Elementary data type (handle).
* ``filetype``: File type (handle). See Restrictions, below.
* ``datarep``: Data representation (string).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The :ref:`MPI_File_get_view` routine returns the process's view of the data in
the file. The current values of the displacement, etype, and filetype
are returned in *disp,* *etype,* and *filetype,* respectively.

The :ref:`MPI_File_get_view` interface allows the user to pass a
data-representation string via the *datarep* argument.


ERRORS
------

.. include:: ./ERRORS.rst
