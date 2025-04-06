.. _mpi_win_get_name:


MPI_Win_get_name
================

.. include_body

:ref:`MPI_Win_get_name` |mdash| Obtains the name of a window.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_get_name.rst

INPUT PARAMETER
---------------
* ``win``: Window whose name is to be returned (handle).

OUTPUT PARAMETERS
-----------------
* ``win_name``: the name previously stored on the window, or an empty string if no such name exists (string).
* ``resultlen``: Length of returned name (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------


ERRORS
------

.. include:: ./ERRORS.rst
