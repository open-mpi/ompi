.. _mpi_win_get_group:


MPI_Win_get_group
=================

.. include_body

:ref:`MPI_Win_get_group` |mdash| Returns a duplicate of the group of the
communicator used to create the window.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_get_group.rst

INPUT PARAMETER
---------------
* ``win``: Window object (handle).

OUTPUT PARAMETERS
-----------------
* ``group``: Group of processes that share access to the window (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_get_group` returns a duplicate of the group of the communicator
used to create the window associated with *win*. The group is returned
in *group*.


ERRORS
------

.. include:: ./ERRORS.rst
